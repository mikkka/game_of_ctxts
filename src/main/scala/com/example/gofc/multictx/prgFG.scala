package com.example.gofc.multictx

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

// we can use only untyped errors here
trait Futa[F[_]] {
  def fromFut(fut: F[Future[String]])(implicit 
    asnc: Async[F],
    _1  : Raise[F,Throwable],
    _2  : Ask[F,String]
  ): F[Int]
}

class FutaImpl[F[_]: Monad] extends Futa[F] {
  def fromFut(fut: F[Future[String]])(implicit 
    asnc: Async[F],
    _1  : Raise[F,Throwable],
    ctx : Ask[F,String]
  ): F[Int] =
    ctx.ask >>= { c => 
      Async[F].fromFuture(fut) map (_.toInt + c.toInt) >>= {x => 
        if (x > 200) TinkyWinky(s"futa tinky $x").raise
        else x.pure[F]
      }
    }
}

// we can use Throwable as unchecked and Buzer as cheched
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

import cats.~>

class PrgFG[G[_]: Monad, F[_]: Monad](
  futa:  Futa[G],
  loopa: Loopa[F],
  console: Cosnole[F]
)(implicit funK: G ~> F) {
  def things(fut: G[Future[String]])(
    implicit
      _g0 : Async[G],
      _g1 : Handle[G, Throwable],
      _g2 : Ask[G,String],

      _f1 : Ask[F, String],
      _f2 : Handle[F, Buzer],
      _f3 : Handle[F, Throwable]
  ): F[String] = 
    funK(futa.fromFut(fut)) >>= {x => 
      loopa.loop(x)
        .handle{e: Throwable => "error: " + e.toString()}
        .handle{e: Buzer =>       s"buzer $e"}
        .flatTap(x => console.prntln("things happened "+ x))
    }
}