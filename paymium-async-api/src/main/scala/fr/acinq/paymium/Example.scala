package fr.acinq.paymium

import scala.util.{Failure, Success}

object Example extends App {

  val client = new PaymiumClient(
    token = "47714b09586e18e98b9cc8e67f7921968b6bc4a7c586e057233d0e36d7642838",
    key = "6b0246d725a71dcf470fb785a90c2bdff98ba746936721dc8cd029949ee717c9"
    )

  import scala.concurrent.ExecutionContext.Implicits.global

  client.ticker() onComplete {
    case Success(res) => println(res)
    case Failure(t: Throwable) => t.printStackTrace()
  }

  client.userinfo() onComplete {
    case Success(res) => println(res)
    case Failure(t: Throwable) => t.printStackTrace()
  }

  /*client.buy(btcAmount = 0.01) onComplete {
    case Success(res) => println(res)
    case Failure(t: Throwable) => t.printStackTrace()
  }

  client.sell(btcAmount = 0.02) onComplete {
    case Success(res) => println(res)
    case Failure(t: Throwable) => t.printStackTrace()
  }*/

}



