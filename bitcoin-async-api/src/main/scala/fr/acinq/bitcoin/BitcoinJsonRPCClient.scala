package fr.acinq.bitcoin

import com.ning.http.client._
import scala.concurrent.{Promise, Future}
import org.json4s.{DefaultReaders, DefaultFormats}
import org.json4s.jackson.Serialization.write
import org.json4s.jackson.JsonMethods.parse
import org.json4s.JsonAST.{JNull, JValue}
import java.io.IOException
import org.json4s.DefaultReaders.{StringReader, IntReader}

case class JsonRPCBody(jsonrpc: String = "1.0", id: String = "scala-client", method: String, params: Seq[Any])

class JsonRPCError(val code: Int, val message: String, val jvalue: JValue) extends IOException(s"$message (code: $code)")

class BitcoinJsonRPCClient(config: AsyncHttpClientConfig, host: String, port: Int, ssl: Boolean) {

  val client: AsyncHttpClient = new AsyncHttpClient(config)

  def this(user: String, password: String, host: String = "127.0.0.1", port: Int = 8332, ssl: Boolean = false) = this(
    new AsyncHttpClientConfig.Builder()
      .setRealm(new Realm.RealmBuilder().setPrincipal(user).setPassword(password).setUsePreemptiveAuth(true).setScheme(Realm.AuthScheme.BASIC).build)
      .build,
    host,
    port,
    ssl
  )

  implicit val formats = DefaultFormats

  def invoke(method: String, params: Any*): Future[JValue] = {
    val promise = Promise[JValue]()
    println()
    client
      .preparePost((if (ssl) "https" else "http") + s"://$host:$port/")
      .addHeader("Content-Type", "application/json")
      .setBody(write(JsonRPCBody(method = method, params = params)))
      .execute(new AsyncCompletionHandler[Unit] {
      override def onCompleted(response: Response): Unit =
        try {
          val jvalue = parse(response.getResponseBody)
          val error = jvalue \ "error"
          val result = jvalue \ "result"
          if (error != JNull) {
            implicit val readers = DefaultReaders
            promise.failure(new JsonRPCError((error \ "code").as[Int], (error \ "message").as[String], jvalue))
          } else {
            promise.success(result)
          }
        } catch {
          case t: Throwable => promise.failure(t)
        }

      override def onThrowable(t: Throwable): Unit = promise.failure(t)
    })
    promise.future
  }

}

