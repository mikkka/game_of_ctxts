package com.example.gofc.prg.baz

import cats.Monad
import cats.syntax.all._
import cats.mtl.Ask
import cats.mtl.syntax.all._
import com.example.gofc.types._
import cats.mtl.Raise

trait MrRed[F[_]] {
  def hello(): F[String]
  def puke(): F[String]
}

class MrRedImpl[F[_]: Monad: Ask[*[_], BazCtx]: Raise[*[_], Throwable]] extends MrRed[F] {
  val baz: BazCtx => String = _.baz

  def hello(): F[String] = 
    baz.reader >>= {baz => s"mr Red greets ${baz}".pure}

  def puke(): F[String] = 
    new Exception("mr Red puked").raise
}