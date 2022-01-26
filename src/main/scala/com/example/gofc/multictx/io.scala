package com.example.gofc.multictx

import cats.Functor
import cats.Monad
import cats.MonadError
import cats._
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.LiftIO
import cats.effect.std.Console
import cats.effect.std.Console._
import cats.mtl.Raise
import cats.syntax.all._

import scala.concurrent.Future

object IOAppa extends IOApp {
  type CtxIO[A] = Kleisli[IO, String, A]
  type CtxErrIO[A] = EitherT[CtxIO, Buzer, A]

  val mona = implicitly[Monad[CtxErrIO]]

  implicit def hellRaiser[F[_]](implicit ME: MonadError[F, Throwable]) = new Raise[F, Throwable] {
    def functor: Functor[F] = ME

    def raise[E2 <: Throwable, A](e: E2): F[A] = ME.raiseError(e)
  }

  val cosnole = new Cosnole[CtxErrIO] {
    def prntln(str: String): CtxErrIO[Unit] = implicitly[Console[CtxErrIO]].println(str)
  }

  // if we use this - all needed errors from CtxIO have been handled already
  implicit val funK: CtxIO ~> CtxErrIO = EitherT.liftK[CtxIO, Buzer]

  val prg = new PrgFG[CtxIO, CtxErrIO](
    new FutaImpl[CtxIO],
    new PrettyLoopa[CtxErrIO],
    cosnole
  )

  def run(args: List[String]): IO[ExitCode] =
    prg
    .things(Future.successful("1").pure[CtxIO])
    .value.run("1")
    .flatTap(x => Console[IO].println("run : " + x))
    .map(_.fold(
      _ => ExitCode.Error,
      _ => ExitCode.Success
    ))
}