# http-client

We had a hard time finding a simple and convenient http client.

This is an attempt at leveraging:
- the awesomeness of ning/async-http-client
- the coolness of spray http model
- the cleverness of dispatch futures

Usage:

```scala
  implicit val c = new AsyncHttpClient()
  implicit val ec =  ExecutionContext.Implicits.global

  import spray.httpx.RequestBuilding._
  import HttpClient._

  for {
    data <- Get("http://google.fr").execute
    q <- Get(Uri("http://google.fr").withQuery(Map("q" -> "abc"))).execute
    json <- Post("https://example.com", FormData(Map("param1" -> "x", "param2" -> "y"))).execute.asJson
    extracted <- Get("http://foobar.com").execute.extractJson[MyClass]
  } yield {
    ...
  }
```
