package fr.acinq.kraken

import com.ning.http.client.AsyncHttpClient

import scala.concurrent.Await
import scala.concurrent.duration._

object Example extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val httpClient = new AsyncHttpClient()
  val client = new KrakenClient("YOUR API KEY", "YOU API SECRET")

  val future = for {
    ticker <- client.ticker(Seq(KrakenClient.CurrencyPair.XBTEUR))
    _ = println(ticker)
    balance <- client.balance
    _ = println(balance)
    response <- client.addOrder(Map("pair" -> "XBTCZEUR", "type" -> "sell", "ordertype" -> "market", "volume" -> "10", "oflags" -> "viqc"))
    _ = println(response)
    txid = response.txid(0)
    info <- client.queryOrders(txid)
    _ = println(info)
    balance1 <- client.balance
  } yield balance1

  val result = Await.result(future, 20 seconds)
  println(result)
  httpClient.close
}
