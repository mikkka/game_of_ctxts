package com.example.gofc.prg.tsk

import cats.Monad
import cats.syntax.all._
import cats.mtl.Raise
import cats.mtl.syntax.all._
import scala.concurrent.Future
import cats.effect.kernel.Async

trait MrWhite[F[_]] {
  def hello(futa: String)(implicit 
    async : Async[F]
  ): F[String]

  def puke(): F[String]
}

class MrWhiteImpl[F[_]: Monad: Raise[*[_], Throwable]] extends MrWhite[F] {
  def hello(futa: String)(implicit 
    async : Async[F]
  ): F[String] = Async[F].delay(futa)
  
  def puke(): F[String] = 
    new Exception("mr White puked").raise
}
