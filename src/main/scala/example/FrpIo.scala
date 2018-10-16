package example

import example.types.IO1
import scalaz._
import scalaz.zio.IO

import scala.concurrent.duration.Duration

package object types {

  type IO1[A] = IO[Void, A]
}

object FrpIo extends Frp[IO1] {
  override def mergeFutures[A](a: Future[A], b: Future[A]): IO1[Future[A]] = {
    println("mergeFutures")
    ???
  }

  override def mergeEvents[A](a: Event[A], b: Event[A]): IO1[Event[A]] = {
    println("mergeEvents")
    ???
  }

  override def delayEvent[A](e: Event[A], interval: Duration): IO1[Event[A]] = {
    println("delayEvent")
    ???
  }

  override def now: IO1[Time] = {
    println("now")
    ???
  }

  override def pure[A](a: A): IO1[A] = IO.sync(a)

  override def future[A](a: A): IO1[Future[A]] = {
    println("future")
    ???
  }

  override def sinkE[A](sink: Sink[A], event: Event[A]): IO1[Unit] = {
    println("sinkE")
    ???
  }
}

object monadIo extends Monad[IO1] {
  override def point[A](a: => A): IO1[A] = {
    println("point")
    ???
  }

  override def bind[A, B](fa: IO1[A])(f: A => IO1[B]): IO1[B] = fa.flatMap(f)
}
