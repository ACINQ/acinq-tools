package fr.acinq.httpclient

import com.ning.http.client._
import org.json4s.jackson.JsonMethods.parse
import org.json4s._
import spray.http._
import spray.http.parser.HttpParser

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

/**
 * Created by PM on 16/01/2015.
 */

object HttpClient {

  implicit def httpReqToExecutor(req: HttpRequest): Executor = new Executor(req)

  implicit def futureResToParsers(f: Future[HttpResponse]): Parsers = new Parsers(f)

}

class Executor(req: HttpRequest) {

  def execute(implicit client: AsyncHttpClient): Future[HttpResponse] = {
    val promise = Promise[HttpResponse]()
    client.executeRequest(req, new AsyncCompletionHandler[Unit] {
      override def onCompleted(response: Response): Unit = promise.success(response)

      override def onThrowable(t: Throwable) = promise.failure(t)
    })
    promise.future
  }

  implicit def sprayReqToAsyncHttpReq(r: HttpRequest): Request = {
    val rb = new RequestBuilder()
      .setMethod(r.method.toString())
      .setUrl(r.uri.toString())
      .setBody(r.entity.data.toByteArray)
    r.headers.foreach(h => rb.addHeader(h.name, h.value))
    r.entity.toOption.map(_.contentType).map(h => rb.addHeader(HttpHeaders.`Content-Type`.name, h.toString()))
    rb.build()
  }

  implicit def asyncHttpResToSprayRes(r: Response): HttpResponse = {
    import scala.collection.JavaConversions._
    val headers: List[HttpHeader] = r.getHeaders.iterator().toList.map(e =>
      e.getValue.map(v => HttpParser.parseHeader(HttpHeaders.RawHeader(e.getKey, v)) match {
        case Right(h) => Some(h)
        case Left(e: ErrorInfo) => None
      })).flatten.flatten
    val entity = headers.find(_.isInstanceOf[HttpHeaders.`Content-Type`]).map(_.asInstanceOf[HttpHeaders.`Content-Type`])
      .map(h => HttpEntity(h.contentType, r.getResponseBodyAsBytes))
      .getOrElse(HttpEntity(r.getResponseBodyAsBytes))

    HttpResponse(
      status = Try(StatusCode.int2StatusCode(r.getStatusCode)).toOption.getOrElse(StatusCodes.registerCustom(r.getStatusCode, "")),
      entity = entity,
      headers = headers
    )
  }
}

class Parsers(f: Future[HttpResponse]) {

  def asJson(implicit ec: ExecutionContext): Future[JValue] = f.map(r => parse(r.entity.asString))

  def extractJson[T](implicit ec: ExecutionContext, format: org.json4s.Formats, manifest: Manifest[T]): Future[T] = f.map(r => parse(r.entity.asString).extract[T])

  def asXml(implicit ec: ExecutionContext): Future[xml.Node] = f.map(r => scala.xml.XML.loadString(r.entity.asString))
}
