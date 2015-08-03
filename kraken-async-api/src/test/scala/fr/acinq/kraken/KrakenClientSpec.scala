package fr.acinq.kraken

import java.io.IOException
import java.nio.file.{Files, Paths}
import java.util.Base64

import com.typesafe.config.ConfigFactory
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

@RunWith(classOf[JUnitRunner])
class KrakenClientSpec extends FlatSpec {

  def readResource(path: String) : String = {
    val url = classOf[KrakenClientSpec].getResource(path)
    new String(Files.readAllBytes(Paths.get(url.toURI())), "UTf-8")
  }

  val config = ConfigFactory.load()
  val apiKey = Try(config.getString("kraken.api-key")).getOrElse("")
  val apiSecret = Try(config.getString("kraken.api-secret")).getOrElse("")

  "Kraken client" should "parse get ticker responses (ok)" in {
    val json = readResource("/get_ticker_response_ok.json")
    val ticker = Ticker.extract(KrakenClient.getResult(json))
    assert(ticker("XXBTZEUR").ask.price === 245.34040)
    assert(ticker("XXBTZEUR").opening_price === 241.78010)
    assert(ticker("XXBTZEUR").number_of_trades === TimeValue(754, 2931))
    assert(ticker("XXBTZJPY").opening_price === 32702.454)
  }
  it should "parse get ticker responses (error)" in {
    val json = readResource("/get_ticker_response_error.json")
    intercept[KrakenClientError] {
      Ticker.extract(KrakenClient.getResult(json))
    }
  }
  it should "parse get balance responses (ok)" in {
    val json = readResource("/get_balance_response_ok.json")
    val balance = Balance.extract(KrakenClient.getResult(json))
    assert(balance("ZEUR") === 48.5953)
  }
  it should "parse add order responses (ok)" in {
    val json = readResource("/add_order_response_ok.json")
    val response = AddOrderResponse.extract(KrakenClient.getResult(json))
    assert(response.txid === Seq("OQAFZY-H5UCR-A4I3E4"))
  }
  it should "handle unparseable responses" in {
    val json = "foobar"
    intercept[RuntimeException] {
      val response = AddOrderResponse.extract(KrakenClient.getResult(json))
    }
  }
  it should "parse add order responses (error)" in {
    val json = readResource("/add_order_response_error.json")
    intercept[KrakenClientError] {
      AddOrderResponse.extract(KrakenClient.getResult(json))
    }
  }
  it should "parse query orders responses (ok)" in {
    val json = readResource("/query_orders_response_ok.json")
    val response = OrderInfo.extract(KrakenClient.getResult(json))
    println(response)
    assert(response("OS2SEF-EAXMB-LQDM6N").cost === 12.79950)
  }
  it should "handle network errors" in {
    val client = new KrakenClient(apiKey, apiSecret, baseUri = "http://nowhere.42")
    intercept[IOException] {
      val result = Await.result(client.ticker(Seq(KrakenClient.CurrencyPair.XBTEUR)), 5 seconds)
    }
  }
  it should "handle API errors" in {
    pending
    val client = new KrakenClient(apiKey, apiSecret)
    intercept[KrakenClientError] {
      val result = Await.result(client.ticker(Seq("foobar")), 5 seconds)
    }
  }
  it should "get ticker from kraken" in {
    pending
    val client = new KrakenClient(apiKey, apiSecret)
    val result = Await.result(client.ticker(Seq(KrakenClient.CurrencyPair.XBTEUR)), 5 seconds)
    println(result)
  }
  it should "buy and sell BTC" in {
    pending
    import ExecutionContext.Implicits.global
    val client = new KrakenClient(apiKey, apiSecret)
    val future = for {
      balance <- client.balance
      _ = println(balance)
      orderId <- client.buy(0.05)
      map <- client.queryOrders(orderId)
      orderInfo = map(orderId)
      _ = println(orderInfo)
      balance1 <- client.balance
      _ = println(balance1)
      orderId1 <- client.sell(0.05)
      map1 <- client.queryOrders(orderId1)
      orderInfo1 = map(orderId1)
      _ = println(orderInfo1)
      balance2 <- client.balance
      _ = println(balance2)
    } yield ()
    Await.ready(future, 15 seconds)
  }
  it should "generate a valid signature" in {
    val apiKey = "foo"
    val apiSecret = new String(Base64.getEncoder.encode("foobar".getBytes("UTF-8")))
    val client = new KrakenClient(apiKey, apiSecret)
    val request = client.buildPrivateRequest("Balance", Map("nonce" -> "1437829049101405200" /*"1436437266579315900"*/))
    val headers = request.headers.map(h => h.name -> h.value).toMap
    assert(headers("Api-Key") === apiKey)
    assert(headers("Api-Sign") === "0kiY7kL3N+4I6+xMjzgO15TP8fLG+G9q893OwwOXmMAmgeJ1q0HBiGSX4dTmjaGsMjZ3381dGfSyk8f0RwxhCg==")
  }
  it should "be thread safe" in {
    val apiKey = "foo"
    val apiSecret = new String(Base64.getEncoder.encode("foobar".getBytes("UTF-8")))
    val client = new KrakenClient(apiKey, apiSecret)
    val request = client.buildPrivateRequest("Balance", Map("nonce" -> "1437829049101405200" /*"1436437266579315900"*/))
    val signature = KrakenClient.sign(apiSecret, request)
    assert(signature === "0kiY7kL3N+4I6+xMjzgO15TP8fLG+G9q893OwwOXmMAmgeJ1q0HBiGSX4dTmjaGsMjZ3381dGfSyk8f0RwxhCg==")

    import scala.concurrent.ExecutionContext.Implicits.global
    val futures = for (i <- 0 to 5000) yield Future(KrakenClient.sign(apiSecret, request))
    val future = Future.sequence(futures)
    val result = Await.result(future, 10 seconds)
    result.map(sig => assert(sig === signature))
  }
}
