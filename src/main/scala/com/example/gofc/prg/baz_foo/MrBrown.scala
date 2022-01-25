package com.example.gofc.prg.baz_foo

import cats.Monad
import cats.syntax.all._
import cats.mtl.Ask
import cats.mtl.syntax.all._
import com.example.gofc.types._
import cats.mtl.Raise

trait MrBrown[F[_]] {
  def hello()(implicit 
    ask: Ask[F, BazCtx],
    raise: Raise[F, MrBrownFail]
  ): F[String]

  def puke(): F[String]
}

case class MrBrownFail(cause: String) extends FooErr

class MrBrownImpl[
  F[_]
  : Monad
  : Raise[*[_], Throwable]
  ] extends MrBrown[F] {
  val baz: BazCtx => String = _.baz

  def hello()(implicit 
    ask: Ask[F, BazCtx],
    raise: Raise[F, MrBrownFail]
  ): F[String] = 
    baz.reader >>= {baz => MrBrownFail(s"fail of ${baz}").raise}

  def puke(): F[String] = 
    new Exception("mr Brown puked").raise
}