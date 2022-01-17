package com.example.gofc.raisehandle

import cats.Functor
import cats.Monad
import cats.MonadError
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.MonadCancel
import cats.effect.std.Console
import cats.effect.std.Console._
import cats.effect.syntax.all._
import cats.instances.all._
import cats.mtl.Ask
import cats.mtl.Handle
import cats.mtl.Raise
import cats.mtl.syntax.all._
import cats.syntax.all._

import scala.util.control.NoStackTrace

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
  console: Console[F]
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
      .flatTap(console.println)
}

object IOAppa extends IOApp {
  type CtxIO[A] = Kleisli[IO, String, A]
  type CtxErrIO[A] = EitherT[CtxIO, Int, A]

  implicit def hellRaiser[F[_]](implicit ME: MonadError[F, Throwable]) = new Raise[F, Throwable] {
    def functor: Functor[F] = ME

    def raise[E2 <: Throwable, A](e: E2): F[A] = ME.raiseError(e)
  }
  
  val prg = new Prg[CtxErrIO](
    new PrettyLoopa[CtxErrIO],
    implicitly[Console[CtxErrIO]]
  )

  override def run(args: List[String]): IO[ExitCode] = {
    val kek = prg.things()
    kek.value.run("100").flatTap(Console[IO].println).map(_.fold(
      _ => ExitCode.Error,
      _ => ExitCode.Success
    ))
  }
}