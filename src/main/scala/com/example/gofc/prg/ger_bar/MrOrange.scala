package com.example.gofc.prg.ger_bar

import cats.Monad
import cats.syntax.all._
import cats.mtl.Ask
import cats.mtl.syntax.all._
import com.example.gofc.types._
import cats.mtl.Raise

trait MrOrange[F[_]] {
  def hello()(implicit
    ask: Ask[F, GerCtx],
    raise: Raise[F, MrOrangeFail]
  ): F[String]
  def puke(): F[String]
}

case class MrOrangeFail(cause: String) extends BarErr

class MrOrangeImpl[
  F[_]
  : Monad
  : Raise[*[_], Throwable]
  ] extends MrOrange[F] {
  val ger: GerCtx => String = _.ger

  def hello()(implicit
    ask: Ask[F, GerCtx],
    raise: Raise[F, MrOrangeFail]
  ): F[String] = 
    ger.reader >>= {baz => MrOrangeFail(s"fail of ${baz}").raise}

  def puke(): F[String] = 
    new Exception("mr Orange puked").raise
}