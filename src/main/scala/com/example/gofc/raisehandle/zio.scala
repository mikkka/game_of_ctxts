package com.example.gofc.raisehandle

import zio.interop.catz._
import zio.interop.catz.mtl._
import cats.effect.std
import cats.MonadError
import cats.mtl.Raise
import cats.Functor
import cats.effect.kernel.Sync
import cats.effect.kernel.Async
import zio.clock.Clock
import zio.ZIO
import zio.URIO
import zio.ExitCode
import zio.ZEnv
import cats.mtl.Handle
import cats.Applicative


object ZIOAppa extends CatsApp {
  type CtxIO[A] = ZIO[Any, Throwable, A]
  type CtxErrIO[A] = ZIO[String, Int, A]
  
  implicit def hellRaiser = new Raise[CtxErrIO, Throwable] {
    def functor: Functor[CtxErrIO] = implicitly

    def raise[E2 <: Throwable, A](e: E2): CtxErrIO[A] = ZIO.die(e)
  }
  
  implicit def hellHandler = new Handle[CtxErrIO, Throwable] {
    def raise[E2 <: Throwable, A](e: E2): CtxErrIO[A] = ZIO.die(e)
    
    def applicative: Applicative[CtxErrIO] = implicitly
    
    def handleWith[A](fa: CtxErrIO[A])(f: Throwable => CtxErrIO[A]): CtxErrIO[A] = 
      fa.catchAllDefect(f)
  }

  val cosnole = new Cosnole[CtxErrIO] {
    def prntln(str: String): CtxErrIO[Unit] = ZIO.effectTotal(println(str)) 
  }

  val prg = new Prg[CtxErrIO](
    new PrettyLoopa[CtxErrIO],
    cosnole
  )

  import zio.console._

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    prg
    .things()
    .provide("kekus").tap(x => putStrLn("run : " + x)).fold(
      _ => ExitCode.failure,
      _ => ExitCode.success
    )
}