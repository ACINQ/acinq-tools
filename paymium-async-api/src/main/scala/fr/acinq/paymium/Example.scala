package fr.acinq.paymium

import scala.util.{Failure, Success}

object Example extends App {

  val client = new PaymiumClient(
    token = "<token>",
    key = "<key>"
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



