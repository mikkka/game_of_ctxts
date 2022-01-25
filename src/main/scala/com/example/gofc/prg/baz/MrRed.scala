package com.example.gofc.prg.baz

import cats.Monad
import cats.effect.kernel.Async
import cats.mtl.Ask
import cats.mtl.Raise
import cats.mtl.syntax.all._
import cats.syntax.all._
import com.example.gofc.types._

import scala.concurrent.Future

trait MrRed[F[_]] {
  def hello()(implicit 
    ask   : Ask[F, BazCtx]
  ): F[String]

  def puke(): F[String]
}

class MrRedImpl[F[_]: Monad : Raise[*[_], Throwable]] extends MrRed[F] {
  val baz: BazCtx => String = _.baz

  def hello()(implicit 
    ask :   Ask[F, BazCtx]
  ): F[String] = baz.reader >>= {baz => s"mr Red greets ${baz}".pure}

  def puke(): F[String] = 
    new Exception("mr Red puked").raise
}