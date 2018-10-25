package example

package approach2

import example.approach2.types.IO1
import scalaz.Scalaz._
import scalaz._
import scalaz.zio._

import scala.concurrent.duration.{Duration, _}
import scala.language.postfixOps

// Approach2: traits

// Issues:
// constructors (future, event etc.) in trait?
// .asInstanceOf in omplementation

trait Future[+A]

trait Reactive[+A] {
  def head: A
  def tail: Event[A]
}

trait Event[+A]

trait Behaviour[+A] // TODO


trait Frp[F[_]] {

  def mergeFutures[A](a: => Future[A], b: Future[A]): F[Future[A]]

  def mergeEvents[A](a: => Event[A], b: Event[A]): F[Event[A]]

  def delayEvent[A](e: => Event[A], interval: Duration): F[Event[A]]

  def now: F[Time]

  def pure[A](a: A): F[A]

  def future[A](a: => A): F[Future[A]]

  def reactive[A](head: A, tail: => Event[A]) : F[Reactive[A]]

  def event[A](f: Future[Reactive[A]]) : F[Event[A]]

  // consumer of event and behaviours
  type Sink[A] = A => F[Unit]

  def sinkE[A](sink: Sink[A], event: Event[A]): F[Unit]
}

object Ops {

  implicit class eventOps[F[_], A](e: Event[A])(implicit frp: Frp[F]) {
    def delay(interval: Duration): F[Event[A]] = frp.delayEvent(e, interval)

    def merge(other: Event[A]): F[Event[A]] = frp.mergeEvents(e, other)
  }


}

object TwoTickers extends App {

  import Ops._

  case class Tick(name: String)

  class Program[F[_]](implicit val frp: Frp[F], m: Monad[F]) {

    def ticks(interval: Duration, name: String): F[Event[Tick]] = {

      val tail: F[Event[Tick]] =  ticks(interval, name).flatMap(e => e.delay(interval))

      for {
        head <- frp.pure(Tick(name))
        _ = println("head")
        tail <- m.bind(ticks(interval, name))(e => e.delay(interval))
        _ = println("tail")
        fr <- frp.reactive(head, tail).flatMap( r => frp.future(r))
      } yield frp.event(fr)

      tail
    }


    def myAppLogic: F[Unit] = {
      val sink = (t: Tick) => frp.pure(println(s"tick ${t.name}"))

      val eventA: F[Event[Tick]] = ticks(0.2 second, "a")
      val eventB: F[Event[Tick]] = ticks(0.1 second, "b")
      val merged: F[Event[Tick]] = m.bind(eventA)(a => m.bind(eventB)(b => a.merge(b)))
      merged.map { e => frp.sinkE(sink, e) }
    }
  }

  implicit val frp = FrpIo
  implicit val m: Monad[IO1] = monadIo

  override def run(args: List[String]): IO[Nothing, TwoTickers.ExitStatus] = {
    val logic: IO1[Unit] = new Program().myAppLogic
    val attempted: IO[Nothing, Either[Void, Unit]] = logic.attempt
    attempted.map(_.fold(_ => 1, _ => 0)).map(ExitStatus.ExitNow(_))
  }
}

package object types {

  type IO1[A] = IO[Void, A]
}

// !! +A
case class FutureIO[A](io: IO1[(Time, A)]) extends Future[A]

// !! +A
case class EventIO[A](value: FutureIO[Reactive[A]]) extends Event[A]

object EventIO {
  def apply[A](f: FutureIO[Reactive[A]]) : EventIO[A] = new EventIO(f)
}

object FrpIo extends Frp[IO1] {
  override def mergeFutures[A](a: => Future[A], b: Future[A]): IO1[Future[A]] = {
    println("mergeFutures")
    ???
  }

  override def mergeEvents[A](a: => Event[A], b: Event[A]): IO1[Event[A]] = {
    println("mergeEvents")
    ???
  }

  override def delayEvent[A](e: => Event[A], interval: Duration): IO1[Event[A]] = {
    println("delayEvent")
    val ea = e.asInstanceOf[EventIO[A]]
    val io: IO1[(Time, Reactive[A])] = ??? // ea.value.io.delay(interval)
    val f = FutureIO(io)
    io.map( i => {
      EventIO(f)
    })
  }

  override def now: IO1[Time] = {
    println("now")
    ???
  }

  override def pure[A](a: A): IO1[A] = IO.sync(a)

  override def future[A](a: => A): IO1[Future[A]] = {
    println("future")
    ???
  }

  override def sinkE[A](sink: Sink[A], event: Event[A]): IO1[Unit] = {
    println("sinkE")
    ???
  }

  override def reactive[A](head: A, tail: => Event[A]): IO1[Reactive[A]] = ???

  override def event[A](f: Future[Reactive[A]]): IO1[Event[A]] = {
    val e: EventIO[A] =  new EventIO(f.asInstanceOf[FutureIO[Reactive[A]]])
    val r :IO1[Event[A]] = IO.sync(e)
    r
  }
}

object monadIo extends Monad[IO1] {
  override def point[A](a: => A): IO1[A] = {
    println("point")
    IO.point(a)
  }

  override def bind[A, B](fa: IO1[A])(f: A => IO1[B]): IO1[B] = fa.flatMap(f)
  override def map[A, B](fa: IO1[A])(f: A => B): IO1[B] = fa.map(f)

}


