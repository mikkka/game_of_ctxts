package com.example.gofc.prg.baz_foo

import cats.Monad
import cats.syntax.all._
import cats.mtl.Ask
import cats.mtl.syntax.all._
import com.example.gofc.types._
import cats.mtl.Raise

trait MrBrown[F[_]] {
  def hello(): F[String]
  def puke(): F[String]
}

case class MrBrownFail(cause: String) extends FooErr

class MrBrownImpl[
  F[_]
  : Monad
  : Ask[*[_], BazCtx]
  : Raise[*[_], Throwable]
  : Raise[*[_], MrBrownFail]
  ] extends MrBrown[F] {
  val baz: BazCtx => String = _.baz

  def hello(): F[String] = 
    baz.reader >>= {baz => MrBrownFail(s"fail of ${baz}").raise}

  def puke(): F[String] = 
    new Exception("mr Brown puked").raise
}