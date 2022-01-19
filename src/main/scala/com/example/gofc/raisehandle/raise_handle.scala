package com.example.gofc.raisehandle

import cats.Functor
import cats.Monad
import cats.effect.syntax.all._
import cats.instances.all._
import cats.mtl.Ask
import cats.mtl.Handle
import cats.mtl.Raise
import cats.mtl.syntax.all._
import cats.syntax.all._

import scala.util.control.NoStackTrace

trait Cosnole[F[_]] {
  def prntln(str: String): F[Unit]
}

trait Loopa[F[_]] {
  def loop()(implicit
    ctx :Ask[F, String],
    _1  :Raise[F, Int],
  ): F[String]
}

case class TinkyWinky(msg: String) extends Exception(msg, null) with NoStackTrace

class PrettyLoopa[F[_]: Monad: Raise[*[_], Throwable]] extends Loopa[F] {
  override def loop()(implicit 
    ctx : Ask[F,String],
    _1  : Raise[F,Int]
  ): F[String] = 
    ((ctx.ask map (_.toInt)) >>= { x => 
      if (x > 88) TinkyWinky(s"$x").raise[F, Int] else x.pure[F]
    } >>= { x => 
      if (x > 14) 88.raise[F, Int] else x.pure[F]
    }).map(_.toString)
}

class Prg[F[_]: Monad](
  loopa: Loopa[F],
  console: Cosnole[F]
) {
  def things()(
    implicit
      _1 : Ask[F, String],
      _2 : Raise[F, Int],
      _3 : Handle[F, Int],
      _4 : Handle[F, Throwable]
  ): F[String] = 
    loopa.loop()
      .handle{e: Throwable => "error: " + e.toString()}
      .handle{e: Int =>       s"epxception $e"}
      .flatTap(x => console.prntln("things happened "+ x))
}

