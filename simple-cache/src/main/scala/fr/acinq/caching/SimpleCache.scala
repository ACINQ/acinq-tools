package fr.acinq.paymium

import spray.caching._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

/**
 * Created by PM on 22/01/2015.
 */
class SimpleCache[T](f: => Future[T], expiration: Duration)(implicit val ec: ExecutionContext) {

  val cache = LruCache[T](maxCapacity = 1, initialCapacity = 1, timeToLive = expiration)

  val KEY = "only-one-entry"

  def get: Future[T] = cache(KEY)(f)

  def refresh: Future[T] = {
    cache.remove(KEY)
    get
  }

}