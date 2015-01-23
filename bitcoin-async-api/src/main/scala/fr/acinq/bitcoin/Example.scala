package fr.acinq.bitcoin

import scala.util.{Failure, Success}
import org.json4s.jackson.JsonMethods._

object Example extends App {

  val client = new BitcoinJsonRPCClient("foo", "bar")

  import scala.concurrent.ExecutionContext.Implicits.global

  client.invoke("getinfo") onComplete {
    case Success(jvalue) => println(pretty(jvalue))
    case Failure(t: Throwable) => t.printStackTrace()
  }

//  client.invoke("sendtoaddress", "msj42CCGruhRsFrGATiUuh25dtxYtnpbTx", 0.2998, "hello 1", "hello 2") onComplete {
//    case Success(jvalue) => println(pretty(jvalue))
//    case Failure(t: Throwable) => t.printStackTrace()
//  }

}



