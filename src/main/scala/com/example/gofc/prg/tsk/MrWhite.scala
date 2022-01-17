package com.example.gofc.prg.tsk

import cats.Monad
import cats.syntax.all._
import cats.mtl.Raise
import cats.mtl.syntax.all._

trait MrWhite[F[_]] {
  def hello(): F[String]
  def puke(): F[String]
}

class MrWhiteImpl[F[_]: Monad: Raise[*[_], Throwable]] extends MrWhite[F] {
  def hello(): F[String] = "white".pure
  
  def puke(): F[String] = 
    new Exception("mr White puked").raise
}
