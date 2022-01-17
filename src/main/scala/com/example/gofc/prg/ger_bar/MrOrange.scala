package com.example.gofc.prg.ger_bar

import cats.Monad
import cats.syntax.all._
import cats.mtl.Ask
import cats.mtl.syntax.all._
import com.example.gofc.types._
import cats.mtl.Raise

trait MrOrange[F[_]] {
  def hello(): F[String]
  def puke(): F[String]
}

case class MrOrangeFail(cause: String) extends FooErr

class MrOrangeImpl[
  F[_]
  : Monad
  : Ask[*[_], BazCtx]
  : Raise[*[_], Throwable]
  : Raise[*[_], MrOrangeFail]
  ] extends MrOrange[F] {
  val baz: BazCtx => String = _.baz

  def hello(): F[String] = 
    baz.reader >>= {baz => MrOrangeFail(s"fail of ${baz}").raise}

  def puke(): F[String] = 
    new Exception("mr Orange puked").raise
}