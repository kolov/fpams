package example

import example.Ops.Sink
import scalaz._, Scalaz._
import scalaz.zio._

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.duration.Duration

case class Future[+A]()

case class Reactive[+A](head: A, tail: Event[A])

case class Event[+A](value: Future[Reactive[A]])

case class Behaviour[+A]()

object Sinks {
  def sinkE[A](sink: Sink[A], value: A) = ???
}

trait Frp[F[_]] {

  def mergeFutures[A](a: Future[A], b: Future[A]): F[Future[A]]

  def mergeEvents[A](a: Event[A], b: Event[A]): F[Event[A]]

  def delayEvent[A](e: Event[A], interval: Duration): F[Event[A]]

  def now: F[Time]

  def pure[A](a: A): F[A]

  def future[A](a: A): F[Future[A]]
}

object Ops {

  implicit class eventOps[F[_], A](e: Event[A])(implicit frp: Frp[F]) {
    def delay(interval: Duration): F[Event[A]] = frp.delayEvent(e, interval)

    def merge(other: Event[A]): F[Event[A]] = frp.mergeEvents(e, other)
  }

  type Sink[A] = A => IO[Void, Unit]
}

object TwoTickers extends App {

  import Ops._

//  type IO1[A] = IO[Void, A]
//  implicit val frp: Frp[IO1] = ???
//  implicit val mio: Monad[IO1] = new Monad[IO1] {
//    override def bind[A, B](fa: IO1[A])(f: A => IO1[B]): IO1[B] = fa.flatMap(f)
//
//    override def point[A](a: => A): IO1[A] = IO.point(a)
//  }
//
//  def run(args: List[String]): IO[Nothing, ExitStatus] =
//    new Program[IO1]().myAppLogic.attempt.map(_.fold(_ => 1, _ => 0)).map(ExitStatus.ExitNow(_))

  case class Tick(name: String)

  class Program[F[_]](implicit val frp: Frp[F], m: Monad[F]) {

    import Ops._

    def ticks[F[_]](interval: Duration, name: String): F[Event[Tick]] = {
      for {
        head <- frp.pure(Tick(name))
        tail <- m.bind(ticks(interval, name))(e => frp.delayEvent(e, interval))
        r = Reactive(head, tail)
        f <- frp.future(r)
      } yield Event(f)
    }


    def myAppLogic: F[Unit] = {
      val sink: Sink[Tick] =
        ( (t: Tick) => IO.now(println(s"tick ${t.name}")) )

      val eventA: F[Event[Tick]] = ticks(0.2 second, "a")
      val eventB: F[Event[Tick]] = ticks(0.1 second, "b")
      val merged: F[Event[Tick]] = m.bind(eventA)(a => m.bind(eventB)(b => frp.mergeEvents(a, b)))
      Sinks.sinkE(sink, merged)
    }
  }

}

