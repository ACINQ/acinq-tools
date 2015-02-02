package fr.acinq.caching

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger

import fr.acinq.retry._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
 * Created by PM on 30/01/2015.
 */
@RunWith(classOf[JUnitRunner])
class RetryTest extends FunSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  test("delay") {
    val delayed = delay(5 seconds)
    intercept[TimeoutException] {
      Await.result(delayed, 3 seconds)
    }
    Await.result(delayed, 10 seconds)
  }

  test("success first time no condition") {
    assert(47 == Await.result(retry(2)(Future.successful(47L)), 30 seconds))
  }

  test("success 3rd time no condition") {
    val count = new AtomicInteger()
    def f = Future {
      count.incrementAndGet()
      if (count.get() == 3) 47L else throw new RuntimeException
    }
    assert(47 == Await.result(retry(3)(f), 30 seconds))
    assert(3 == count.get())
  }

  test("failed 2 tries time no condition") {
    val count = new AtomicInteger()
    def f = Future {
      count.incrementAndGet()
      if (count.get() == 3) 47L else throw new RuntimeException
    }
    intercept[RuntimeException] {
      assert(47 == Await.result(retry(2)(f), 30 seconds))
    }
    assert(2 == count.get())
  }

  test("success first time condition met") {
    assert(47 == Await.result(retryUntil(2)(Future.successful(47L))(x => true), 30 seconds))
  }

  test("always success condition met on 3rd time") {
    val count = new AtomicInteger()
    def f = Future {
      count.incrementAndGet()
    }
    assert(3 == Await.result(retryUntil(3)(f)(_ == 3), 30 seconds))
    assert(3 == count.get())
  }

  test("always success condition never met") {
    val count = new AtomicInteger()
    def f = Future {
      count.incrementAndGet()
    }
    intercept[ConditionsNotMetException[Int]] {
      assert(3 == Await.result(retryUntil(3)(f)(_ == 4), 30 seconds))
    }
    assert(3 == count.get())
  }

  test("fails right away") {
    val count = new AtomicInteger()
    def f = Future[Int] {
      throw new RuntimeException
    }
    intercept[RuntimeException] {
      Await.result(retryUntil(3, 2 seconds)(f)(_ == 4), 30 seconds)
    }
  }

}
