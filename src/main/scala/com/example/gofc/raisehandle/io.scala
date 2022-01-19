package com.example.gofc.raisehandle

import cats.Functor
import cats.Monad
import cats.MonadError
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.effect.std.Console._
import cats.mtl.Raise

object IOAppa extends IOApp {
  type CtxIO[A] = Kleisli[IO, String, A]
  type CtxErrIO[A] = EitherT[CtxIO, Int, A]

  implicit def hellRaiser[F[_]](implicit ME: MonadError[F, Throwable]) = new Raise[F, Throwable] {
    def functor: Functor[F] = ME

    def raise[E2 <: Throwable, A](e: E2): F[A] = ME.raiseError(e)
  }

  val cosnole = new Cosnole[CtxErrIO] {
    def prntln(str: String): CtxErrIO[Unit] = implicitly[Console[CtxErrIO]].println(str)
  }
  
  val prg = new Prg[CtxErrIO](
    new PrettyLoopa[CtxErrIO],
    cosnole
  )

  def run(args: List[String]): IO[ExitCode] =
    prg
    .things()
    .value.run("kekus").flatTap(x => Console[IO].println("run : " + x)).map(_.fold(
      _ => ExitCode.Error,
      _ => ExitCode.Success
    ))
}