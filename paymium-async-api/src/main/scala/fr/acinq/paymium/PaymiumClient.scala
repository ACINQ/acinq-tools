package fr.acinq.paymium

import java.io.IOException
import java.text.SimpleDateFormat
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import com.ning.http.client._
import fr.acinq.httpclient.HttpClient._
import spray.httpx.RequestBuilding
import RequestBuilding._
import org.apache.commons.codec.binary.Hex
import spray.http._
import scala.concurrent.{ExecutionContext, Future}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import StatusCodes._

case class Credentials(token: String, key: String)

class PaymiumClientError(val status: Int, val error: Error) extends IOException(s"Paymium API returned an error (status: $status, reason: ${error.errors.mkString(",")}")

class PaymiumClient(credentials_opt: Option[Credentials] = None)(implicit client: AsyncHttpClient = new AsyncHttpClient(), ec: ExecutionContext = ExecutionContext.Implicits.global) {

  def this(token: String, key: String) = this(Some(Credentials(token, key)))

  val sha256_HMAC_opt = credentials_opt.map(credentials => {
    val mac = Mac.getInstance("HmacSHA256")
    val secret_key = new SecretKeySpec(credentials.key.getBytes(), "HmacSHA256")
    mac.init(secret_key)
    mac
  })

  implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  }

  def handleError(response: Future[HttpResponse]) = response.flatMap(_ match {
    case HttpResponse(status, _, _, _) if status == OK || status == Created => response
    case HttpResponse(status, body, _, _) => throw new PaymiumClientError(status.intValue, parse(body.asString).extract[Error])
  })

  def sign(request: HttpRequest) = {
    assert(sha256_HMAC_opt.isDefined)
    val (sig, nonce) = synchronized {
      val nonce = System.nanoTime()
      // hmac is not thread safe
      val sig = Hex.encodeHexString(sha256_HMAC_opt.get.doFinal((nonce + request.uri.toString() + request.entity.asString).getBytes))
      // nonce must be increased at each call
      (sig, nonce)
    }
    request.copy(headers =
        HttpHeaders.RawHeader("Api-Key", credentials_opt.get.token) ::
        HttpHeaders.RawHeader("Api-Signature", sig) ::
        HttpHeaders.RawHeader("Api-Nonce", nonce.toString) :: request.headers)
  }

  def ticker(): Future[Ticker] = {
    handleError(Get("https://paymium.com/api/v1/data/eur/ticker").execute).extractJson[Ticker]
  }

  def userinfo(): Future[UserInfo] = {
    require(sha256_HMAC_opt.isDefined, "token credentials need to be set for this call")
    handleError(sign(Get("https://paymium.com/api/v1/user")).execute).extractJson[UserInfo]
  }

  val decimalFormat = new java.text.DecimalFormat("#.########")

  def buy(btcAmount: Double): Future[Order] = {
    require(sha256_HMAC_opt.isDefined, "token credentials need to be set for this call")
    handleError(sign(Post("https://paymium.com/api/v1/user/orders",
      FormData(Map("type" -> "MarketOrder", "currency" -> "EUR", "direction" -> "buy", "amount" -> decimalFormat.format(btcAmount))))).execute).extractJson[Order]
  }

  def sell(btcAmount: Double): Future[Order] = {
    require(sha256_HMAC_opt.isDefined, "token credentials need to be set for this call")
    handleError(sign(Post("https://paymium.com/api/v1/user/orders",
      FormData(Map("type" -> "MarketOrder", "currency" -> "EUR", "direction" -> "sell", "amount" -> decimalFormat.format(btcAmount)))))execute).extractJson[Order]
  }


}
