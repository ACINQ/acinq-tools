package fr.acinq.kraken

import java.io.IOException
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import com.ning.http.client._
import fr.acinq.httpclient.HttpClient._
import org.json4s.JsonAST.{JDouble, JString}
import org.json4s.{JValue, DefaultFormats}
import org.json4s.jackson.JsonMethods
import spray.http._
import spray.httpx.RequestBuilding._

import scala.compat.Platform
import scala.concurrent.{ExecutionContext, Future}

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

  private case class RawCurrencyPairTicker(a: Seq[String], b: Seq[String], c: Seq[String], v: Seq[String], p: Seq[String], t: Seq[Int], l: Seq[String], h: Seq[String], o: String)

  private case class RawTickerResponse(error: Seq[String], result: Map[String, RawCurrencyPairTicker] = Map.empty[String, RawCurrencyPairTicker])

  case class PriceVolume(price: Double, volume: Double)

  object PriceVolume {
    def apply(raw: Seq[String]) = new PriceVolume(raw(0).toDouble, raw(1).toDouble)
  }

  case class TimeValue(today: Double, last_24_hours: Double)

  object TimeValue {
    def apply(raw: Seq[String]) = new TimeValue(raw(0).toDouble, raw(1).toDouble)
  }

  case class CurrencyPairTicker(ask: PriceVolume,
                                bid: PriceVolume,
                                last_trade_closed: PriceVolume,
                                volume: TimeValue,
                                volume_weighted_average_price: TimeValue,
                                number_of_trades: TimeValue,
                                low: TimeValue,
                                high: TimeValue,
                                opening_price: Double)

  object CurrencyPairTicker {
    def apply(raw: RawCurrencyPairTicker) = new CurrencyPairTicker(ask = PriceVolume(raw.a),
      bid = PriceVolume(raw.b),
      last_trade_closed = PriceVolume(raw.c),
      volume = TimeValue(raw.v),
      volume_weighted_average_price = TimeValue(raw.p),
      number_of_trades = TimeValue(raw.t(0).toDouble, raw.t(1).toDouble),
      low = TimeValue(raw.l),
      high = TimeValue(raw.h),
      opening_price = raw.o.toDouble)
  }

  type Ticker = Map[String, CurrencyPairTicker]

  object Ticker {
    def parse(json: String): Ticker = {
      val raw = JsonMethods.parse(json).extract[RawTickerResponse]
      if(!raw.error.isEmpty) throw new KrakenClientError(StatusCodes.OK.intValue, raw.error)
      raw.result.mapValues(r => CurrencyPairTicker(r))
    }
  }

  private case class RawBalanceResponse(error: Seq[String], result: Map[String, String])

  type Balance = Map[String, Double]

  object Balance {
    def parse(json: String): Map[String, Double] = {
      val raw = JsonMethods.parse(json).extract[RawBalanceResponse]
      if(!raw.error.isEmpty) throw new KrakenClientError(StatusCodes.OK.intValue, raw.error)
      raw.result.mapValues(_.toDouble)
    }
  }

  object AddOrderResponse {

    private[KrakenClient] case class Result(descr: Map[String, String], txid: Seq[String])

    def parse(json: String): AddOrderResponse = {
      val raw = JsonMethods.parse(json).extract[RawAddOrderResponse]
      if(!raw.error.isEmpty) throw new KrakenClientError(StatusCodes.OK.intValue, raw.error)
      AddOrderResponse(raw.result.get.descr, raw.result.get.txid)
    }
  }

  private case class RawAddOrderResponse(error: Seq[String], result: Option[AddOrderResponse.Result])

  case class AddOrderResponse(descr: Map[String, String], txid: Seq[String])

  object OrderInfo {
    private case class RawOrderInfo(error: Seq[String], result: Map[String, JValue])

    def parse(json: String): Map[String, OrderInfo] = {
      val raw = JsonMethods.parse(json).transformField({
        case (k, JString(v)) if k == "cost" | k == "fee" | k == "price" | k == "vol" | k == "vol_exec" => (k, JDouble(v.toDouble))
      }).extract[RawOrderInfo]
      if(!raw.error.isEmpty) throw new KrakenClientError(StatusCodes.OK.intValue, raw.error)
      raw.result.mapValues(_.extract[OrderInfo])
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

  private def handleError(response: Future[HttpResponse]) = response.flatMap(_ match {
    case HttpResponse(status, _, _, _) if status == StatusCodes.OK => response
    case HttpResponse(status, body, _, _) => throw new KrakenClientError(status.intValue)
  })

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
    handleError(buildPublicRequest("Ticker", Map("pair" -> currencyPairs.mkString(","))).execute).map(r => Ticker.parse(r.entity.asString))
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
    handleError(buildPrivateRequest("Balance", Map.empty[String, String]).execute).map(r => Balance.parse(r.entity.asString))
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
    handleError(buildPrivateRequest("AddOrder", parameters).execute).map(r => AddOrderResponse.parse(r.entity.asString))
  }

  /**
   * query the status of an order
   * @param txid
   * @return
   */
  def queryOrders(txid: String): Future[Map[String, OrderInfo]] = {
    handleError(buildPrivateRequest("QueryOrders", Map("txid" -> txid)).execute).map(r => OrderInfo.parse(r.entity.asString))
  }
}
