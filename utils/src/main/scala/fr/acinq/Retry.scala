package fr.acinq

import akka.actor.ActorSystem

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
 * Created by PM on 30/01/2015.
 */
package object retry {

  private val actorSystem = ActorSystem()
  private val scheduler = actorSystem.scheduler

  def delay(interval: FiniteDuration)(implicit ec: ExecutionContext): Future[Unit] = {
    val p = Promise[Unit]()
    scheduler.scheduleOnce(interval)(p.success())
    p.future
  }

  case class ConditionsNotMetException[T](lastResult: T)(implicit mf: Manifest[T]) extends RuntimeException

  /**
   * This function is a helper that will retry an operation until the it completes successfully and a condition is met on the result
   * @param n maximum number of tries
   * @param interval interval between two calls
   * @param f main operation returning a future
   * @param until condition on the result of the future
   * @return
   */
  def retryUntil[T](n: Int, interval: FiniteDuration = 0 seconds)(f: => Future[T])(until: T => Boolean)(implicit mf: Manifest[T], ec: ExecutionContext): Future[T] = {
    f flatMap (_ match {
      case r if until(r) => Future.successful(r)
      case r => throw new ConditionsNotMetException[T](r)
    }) recoverWith {
      case _ if n > 1 => delay(interval).flatMap(x => retryUntil(n - 1, interval)(f)(until))
    }
  }

  /**
   * This function is a helper that will retry an operation until the it completes successfully
   * @param n maximum number of tries
   * @param interval interval between two calls
   * @param f main operation returning a future
   * @return
   */
  def retry[T](n: Int, interval: FiniteDuration = 0 seconds)(f: => Future[T])(implicit mf: Manifest[T], ec: ExecutionContext): Future[T] = retryUntil[T](n, interval)(f)(T => true)


}

