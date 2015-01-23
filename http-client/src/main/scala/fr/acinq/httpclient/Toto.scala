package fr.acinq.httpclient

import spray.http.Uri

/**
 * Created by pierre-marie on 1/19/2015.
 */
object Toto extends App {
  import spray.httpx.RequestBuilding._

  Uri("https://www.google.com/recaptcha/api/siteverify").withQuery(Map("secret" -> "abc", "response" -> "r", "remoteip" -> "1.2.3.4"))

}
