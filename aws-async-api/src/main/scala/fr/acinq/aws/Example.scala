package fr.acinq.bitcoin

import fr.acinq.aws.ses.Email

import scala.concurrent.Await

object Example extends App {

  System.setProperty("aws.accessKeyId", "")
  System.setProperty("aws.secretKey", "")

  import fr.acinq.aws.Implicits.EU_WEST_1.ses
  import scala.concurrent.duration._

  Await.result(Email(from = "noreply@flipcoin.fr", to = "pm@flipcoin.fr", subject = "test", text = "text content", html = "<html><p>Hello !</p></html>").send(), 5 seconds)

}



