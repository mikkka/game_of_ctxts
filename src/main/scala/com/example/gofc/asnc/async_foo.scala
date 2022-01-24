package com.example.gofc.asnc

import cats.Monad
import cats.effect.syntax.all._
import cats.mtl.Ask
import cats.mtl.Handle
import cats.mtl.Raise
import cats.mtl.syntax.all._
import cats.syntax.all._

import scala.util.control.NoStackTrace
import cats.effect.kernel.Async
import scala.concurrent.Future


trait Cosnole[F[_]] {
  def prntln(str: String): F[Unit]
}

case class TinkyWinky(msg: String) extends Exception(msg, null) with NoStackTrace
case class Buzer(msg: String)
// case class Buzer(msg: String) extends Exception(msg, null) with NoStackTrace

trait Futa[F[_]] {
  def fromFut(fut: F[Future[String]])(implicit 
    asnc: Async[F],
    _1  : Raise[F,Buzer]
  ): F[Int]
}

class FutaImpl[F[_]: Monad: Raise[*[_], Throwable]] extends Futa[F] {
  def fromFut(fut: F[Future[String]])(implicit 
    asnc: Async[F],
    _1  : Raise[F,Buzer]
  ): F[Int] =
    Async[F].fromFuture(fut) map (_.toInt) >>= {x => 
      if (x > 200) Buzer("bigger than 200").raise
      else if (x > 300) TinkyWinky(s"futa tinky $x").raise
      else x.pure[F]
    }
}

trait Loopa[F[_]] {
  def loop(start: Int)(implicit
    ctx :Ask[F,String],
    _1  :Raise[F,Buzer],
  ): F[String]
}

class PrettyLoopa[F[_]: Monad: Raise[*[_], Throwable]] extends Loopa[F] {
  override def loop(start: Int)(implicit 
    ctx : Ask[F,String],
    _1  : Raise[F,Buzer]
  ): F[String] = 
    ((ctx.ask map (_.toInt + start)) >>= { x => 
      if (x > 88) TinkyWinky(s"loopa winky $x").raise[F, Int] else x.pure
    } >>= { x => 
      if (x > 14) Buzer("bigger than 14").raise[F, Int] else x.pure
    }).map(_.toString)
}


class PrgF[F[_]: Monad](
  futa:  Futa[F],
  loopa: Loopa[F],
  console: Cosnole[F]
) {
  def things(fut: F[Future[String]])(
    implicit
      _0 : Async[F],
      _1 : Ask[F, String],
      _2 : Handle[F, Buzer],
      _3 : Handle[F, Throwable]
  ): F[String] = 
    futa.fromFut(fut) >>= {x => 
      loopa.loop(x)
        .handle{e: Throwable => "error: " + e.toString()}
        .handle{e: Buzer =>       s"buzer $e"}
        .flatTap(x => console.prntln("things happened "+ x))
    }
}