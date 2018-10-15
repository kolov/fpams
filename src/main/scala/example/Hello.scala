package example

import scalaz._, Scalaz._
import scalaz.zio._

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.duration.Duration

case class Future[+A]()

case class Reactive[+A](head: A, tail: Event[A])

case class Event[+A](value: Future[Reactive[A]])

case class Behaviour[+A]()


trait Frp[F[_]] {

  type Sink[A] = A => F[Unit]

  def mergeFutures[A](a: Future[A], b: Future[A]): F[Future[A]]

  def mergeEvents[A](a: Event[A], b: Event[A]): F[Event[A]]

  def delayEvent[A](e: Event[A], interval: Duration): F[Event[A]]

  def now: F[Time]

  def pure[A](a: A): F[A]

  def future[A](a: A): F[Future[A]]

  def sinkE[A](sink: Sink[A], value: A): F[Unit]
}

object Ops {

  implicit class eventOps[F[_], A](e: Event[A])(implicit frp: Frp[F]) {
    def delay(interval: Duration): F[Event[A]] = frp.delayEvent(e, interval)

    def merge(other: Event[A]): F[Event[A]] = frp.mergeEvents(e, other)
  }


}

object TwoTickers extends App {

  case class Tick(name: String)

  class Program[F[_]](implicit val frp: Frp[F], m: Monad[F]) {

    def ticks(interval: Duration, name: String): F[Event[Tick]] = {
      for {
        head <- frp.pure(Tick(name))
        tail <- m.bind(ticks(interval, name))(e => frp.delayEvent(e, interval))
        r = Reactive(head, tail)
        f <- frp.future(r)
      } yield Event(f)
    }


    def myAppLogic: F[Unit] = {
      val sink = (t: Tick) => frp.pure(println(s"tick ${t.name}"))

      val eventA: F[Event[Tick]] = ticks(0.2 second, "a")
      val eventB: F[Event[Tick]] = ticks(0.1 second, "b")
      val merged: F[Event[Tick]] = m.bind(eventA)(a => m.bind(eventB)(b => frp.mergeEvents(a, b)))
      frp.sinkE(sink, merged)
    }
  }

}

