package com.example.gofc.prg.ger

import cats.Monad
import cats.syntax.all._
import cats.mtl.Ask
import cats.mtl.syntax.all._
import com.example.gofc.types._
import cats.mtl.Raise

trait MrGreen[F[_]] {
  def hello(): F[String]
  def puke(): F[String]
}

class MrGreenImpl[F[_]: Monad: Ask[*[_], GerCtx]: Raise[*[_], Throwable]] extends MrGreen[F] {
  val ger: GerCtx => String = _.ger

  def hello(): F[String] = 
    ger.reader >>= {ger => s"mr Green greets ${ger}".pure}
  
  def puke(): F[String] = 
    new Exception("mr Green puked").raise
}
