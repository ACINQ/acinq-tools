package fr.acinq.kraken

import java.io.IOException
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import com.ning.http.client._
import fr.acinq.httpclient.HttpClient._
import org.json4s.JValue
import org.json4s.JsonAST._
import org.json4s.{JValue, DefaultFormats}
import org.json4s.jackson.JsonMethods
import spray.http._
import spray.httpx.RequestBuilding._

import scala.compat.Platform
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class PriceVolume(price: Double, volume: Double)

case class TimeValue(today: Double, last_24_hours: Double)

case class CurrencyPairTicker(ask: PriceVolume,
                              bid: PriceVolume,
                              last_trade_closed: PriceVolume,
                              volume: TimeValue,
                              volume_weighted_average_price: TimeValue,
                              number_of_trades: TimeValue,
                              low: TimeValue,
                              high: TimeValue,
                              opening_price: Double)

object Balance {
  def extract(result: JValue): Map[String, Double] = {
    implicit val formats = new DefaultFormats {
      override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    }
    result.extract[Map[String, String]].mapValues(_.toDouble)
  }
}

object Ticker {
  def extract(result: JValue): Ticker = {
    implicit val formats = new DefaultFormats {
      override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    }
    val result1 = result.transformField {
      case ("a", JArray(JString(v1) :: JString(v2) :: _)) => ("ask", JObject(List(("price", JDouble(v1.toDouble)), ("volume", JDouble(v2.toDouble)))))
      case ("b", JArray(JString(v1) :: JString(v2) :: _)) => ("bid", JObject(List(("price", JDouble(v1.toDouble)), ("volume", JDouble(v2.toDouble)))))
      case ("c", JArray(JString(v1) :: JString(v2) :: Nil)) => ("last_trade_closed", JObject(List(("price", JDouble(v1.toDouble)), ("volume", JDouble(v2.toDouble)))))
      case ("v", JArray(JString(v1) :: JString(v2) :: Nil)) => ("volume", JObject(List(("today", JDouble(v1.toDouble)), ("last_24_hours", JDouble(v2.toDouble)))))
      case ("p", JArray(JString(v1) :: JString(v2) :: Nil)) => ("volume_weighted_average_price", JObject(List(("today", JDouble(v1.toDouble)), ("last_24_hours", JDouble(v2.toDouble)))))
      case ("t", JArray(JInt(v1) :: JInt(v2) :: Nil)) => ("number_of_trades", JObject(List(("today", JDouble(v1.toDouble)), ("last_24_hours", JDouble(v2.toDouble)))))
      case ("l", JArray(JString(v1) :: JString(v2) :: Nil)) => ("low", JObject(List(("today", JDouble(v1.toDouble)), ("last_24_hours", JDouble(v2.toDouble)))))
      case ("h", JArray(JString(v1) :: JString(v2) :: Nil)) => ("high", JObject(List(("today", JDouble(v1.toDouble)), ("last_24_hours", JDouble(v2.toDouble)))))
      case ("o", JString(v)) => ("opening_price", JDouble(v.toDouble))
    }
    result1.extract[Map[String, CurrencyPairTicker]]
  }
}

object OrderInfo {
  implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  }

  def extract(result: JValue): Map[String, OrderInfo] = {
    result.transformField({
      case (k, JString(v)) if k == "cost" | k == "fee" | k == "price" | k == "vol" | k == "vol_exec" => (k, JDouble(v.toDouble))
    }).extract[Map[String, JValue]].mapValues(_.extract[OrderInfo])
  }
}

case class OrderInfo(refid: String,
                     userref: String,
                     status: String,
                     reason: String,
                     opentm: Double,
                     closetm: Double,
                     starttm: Double,
                     expiretm: Double,
                     descr: Map[String, String],
                     vol: Double,
                     vol_exec: Double,
                     cost: Double,
                     fee: Double,
                     price: Double,
                     misc: String,
                     oflags: String)

object AddOrderResponse {
  implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  }

  def extract(result: JValue): AddOrderResponse = {
    result.extract[AddOrderResponse]
  }
}

case class AddOrderResponse(descr: Map[String, String], txid: Seq[String])

class KrakenClientError(val status: Int, val error: Seq[String] = Seq.empty[String]) extends IOException(s"Kraken API returned an error (status: $status, reason: ${error.mkString(",")}")

object KrakenClient {
  implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  }

  object CurrencyPair {
    val XBTEUR = "XBTEUR"
    val XBTJPY = "XBTJPY"
    val XBTUSD = "XBTUSD"
    val XBTGBP = "XBTGBP"
    val LTCEUR = "LTCEUR"
    val LTCUSD = "LTCUSD"
    val XBTLTC = "XBTLTC"
    val XBTNMC = "XBTNMC"
    val XBTSTR = "XBTSTR"
    val XBTXDG = "XBTXDG"
    val XBTXRP = "XBTXRP"
    val XBTXVN = "XBTXVN"
    val XBTCAD = "XBTCAD"
  }

  case class JsonRpcResponse(error: Seq[String], result: JValue)

  def getResult(json: String) : JValue = Try(JsonMethods.parse(json).extract[JsonRpcResponse]) match {
    case Success(JsonRpcResponse(error, result)) if error.isEmpty => result
    case Success(JsonRpcResponse(error, _)) => throw new KrakenClientError(200, error)
    case Failure(t) => throw new IllegalArgumentException(s"cannot parse $json", t)
  }

  def sha256(input: Seq[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-256")
    md.digest(input.toArray)
  }

  def sha256(input: String): Array[Byte] = sha256(input.getBytes("UTF-8"))

  def sign(privateKey: Array[Byte], request: HttpRequest): String = {
    val formdata = FormData(Uri.Query(request.entity.asString)).fields.toMap
    val nonce = formdata("nonce")
    val postdata = request.entity.asString
    val hash = sha256(nonce + postdata)
    val mac = Mac.getInstance("HmacSHA512")
    val key = new SecretKeySpec(privateKey, "HmacSHA512")
    mac.init(key)
    val uripath = request.uri.path.toString()
    mac.update(uripath.getBytes("UTF-8"))
    val raw = mac.doFinal(hash)
    Base64.getEncoder.encodeToString(raw)
  }

  def sign(privateKey: String, request: HttpRequest): String = sign(Base64.getDecoder.decode(privateKey), request)

  def makeNonce: String = (Platform.currentTime * 1000000L).toString
}

/**
 *
 * @param apiKey user's kraken API key
 * @param apiSecret user's kraken API secret
 * @param baseUri kraken API url (default is https://api.kraken.com/0)
 * @param client AsyncHttpClient instance
 * @param ec execution context
 */
class KrakenClient(apiKey: String, apiSecret: String, baseUri: String = "https://api.kraken.com/0")(implicit client: AsyncHttpClient = new AsyncHttpClient(), ec: ExecutionContext = ExecutionContext.Implicits.global) {

  import KrakenClient._

  lazy val apiSecretKey = Base64.getDecoder.decode(apiSecret)

  def call(request: HttpRequest): Future[JValue]  = {
    request.execute.map(response => response match {
      case HttpResponse(StatusCodes.OK, body, _, _) => KrakenClient.getResult(body.asString)
      case HttpResponse(status, body, _, _) => throw new KrakenClientError(status.intValue, Seq(body.asString))
    })
  }

  /**
   * builds an http request that can be used with an async http client
   * public API requests are not user specific, API keys and secrets are not used
   * {{{
   *   import fr.acinq.httpclient.HttpClient._
   *   val request = client.buildPublicRequest("Ticker", Map("pair" -> CurrencyPair.XBTEUR))
   *   val future: Future[HttpResponse] = request.execute
   * }}}
   * @param method name of the method to be called
   * @param parameters method parameters
   * @return a [[spray.http.HttpRequest]]
   */
  def buildPublicRequest(method: String, parameters: Map[String, String]): HttpRequest = Get(Uri(s"$baseUri/public/$method").withQuery(parameters))

  /**
   * return ticker information
   * @param currencyPairs list of currency pairs (defaults to Seq(CurrencyPair.XBTEUR))
   * @return
   */
  def ticker(currencyPairs: Seq[String] = Seq(CurrencyPair.XBTEUR)): Future[Ticker] = {
    call(buildPublicRequest("Ticker", Map("pair" -> currencyPairs.mkString(",")))).map(Ticker.extract)
  }

  /**
   * builds an http request that can be used with an async http client
   * private API requests are user specific, the user being identified by their API keys and secrets
   * {{{
   *   import fr.acinq.httpclient.HttpClient._
   *   val request = client.buildPrivateRequest("AddOrder", Map("pair" -> "XBTCZEUR", "type" -> "sell", "ordertype" -> "market", "volume" -> "0.05"))
   *   val future: Future[HttpResponse] = request.execute
   * }}}
   * @param method name of the method to be called
   * @param parameters method parameters
   * @return a [[spray.http.HttpRequest]]
   */
  def buildPrivateRequest(method: String, parameters: Map[String, String]): HttpRequest = {
    val parameters1 = parameters.get("nonce") match {
      case Some(_) => parameters
      case None => parameters + ("nonce" -> makeNonce)
    }
    val request = Post(s"$baseUri/private/$method", FormData(parameters1))
    val signature = sign(apiSecretKey, request)
    request.copy(headers = HttpHeaders.RawHeader("Api-Key", apiKey) :: HttpHeaders.RawHeader("Api-Sign", signature) :: request.headers)
  }

  /**
   * retrieve the user's balance
   * @return a [[scala.concurrent.Future[Balance] ]]
   */
  def balance: Future[Balance] = {
    call(buildPrivateRequest("Balance", Map.empty[String, String])).map(Balance.extract)
  }

  /**
   * register a new trade order
   * see https://www.kraken.com/help/api#add-standard-order
   * {{{
   *  // sell 0.05 BTC
   *  client.addOrder(Map("pair" -> "XBTCZEUR", "type" -> "sell", "ordertype" -> "market", "volume" -> "0.05"))
   *  // buy 0.05 BTC
   *  client.addOrder(Map("pair" -> "XBTCZEUR", "type" -> "buy", "ordertype" -> "market", "volume" -> "0.05"))
   *  // sell 10 euros worth of BTC
   *  client.addOrder(Map("pair" -> "XBTCZEUR", "type" -> "sell", "ordertype" -> "market", "volume" -> "10", "oflags" -> "viqc"))
   *  // buy 10 euros worth of BTC
   *  client.addOrder(Map("pair" -> "XBTCZEUR", "type" -> "buy", "ordertype" -> "market", "volume" -> "10", "oflags" -> "viqc"))
   * }}}
   * @return a Future[AddOrderResponse]
   */
  def addOrder(parameters: Map[String, String]): Future[AddOrderResponse] = {
    call(buildPrivateRequest("AddOrder", parameters)).map(AddOrderResponse.extract)
  }

  /**
   * buy btcAmout BTC
   * @param btcAmount amount of BTC to buy
   * @param ec
   * @return a Future[String] where the string will be the order id
   */
  def buy(btcAmount: Double)(implicit ec: ExecutionContext): Future[String] = {
    addOrder(Map("pair" -> "XBTCZEUR", "type" -> "buy", "ordertype" -> "market", "volume" -> btcAmount.toString)).map(_.txid(0))
  }

  /**
   * sell btcAmount BTC
   * @param btcAmount amount of BTC to sell
   * @param ec
   * @return a Future[String] where the string will be the order id
   */
  def sell(btcAmount: Double)(implicit ec: ExecutionContext): Future[String] = {
    addOrder(Map("pair" -> "XBTCZEUR", "type" -> "sell", "ordertype" -> "market", "volume" -> btcAmount.toString)).map(_.txid(0))
  }

  /**
   * query the status of an order
   * @param txid
   * @return
   */
  def queryOrders(txid: String): Future[Map[String, OrderInfo]] = {
    call(buildPrivateRequest("QueryOrders", Map("txid" -> txid))).map(OrderInfo.extract)
  }
}
