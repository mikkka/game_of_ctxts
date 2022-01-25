package com.example.gofc.multictx

import zio.interop.catz._
import cats.syntax.all._
import zio.interop.catz.mtl._
import cats.effect.std
import cats.MonadError
import cats.mtl.Raise
import cats.Functor
import cats.effect.kernel.Sync
import cats.effect.kernel.Async
import zio.ZIO
import zio.URIO
import zio.ExitCode
import zio.ZEnv
import cats.mtl.Handle
import cats.Applicative

import zio.blocking._
import zio.clock._
import cats.data.EitherT
import scala.concurrent.Future
import zio.ZLayer
import zio.Has
import zio.Task
import cats.mtl.Ask
import izumi.reflect.Tag
import cats._

object ZIOAppa extends CatsApp {
  type Ctx = Clock with Blocking with Has[String]
  type CtxIO[A] = ZIO[Ctx, Throwable, A]
  type CtxErrIO[A] = ZIO[Ctx, Buzer, A]

  def askFromHas[R, E, A](
    implicit 
    app   : Applicative[ZIO[R,E,*]],
    askHas: Ask[ZIO[R, E, *], Has[A]],
    atag  : Tag[A]
  ): Ask[ZIO[R, E, *], A] = 
    new Ask[ZIO[R, E, *], A] {
      override def applicative: Applicative[ZIO[R,E,*]] = app

      override def ask[A1 >: A]: ZIO[R, E, A1] = 
        askHas.ask[Has[A]].map{hasA: Has[A] => hasA.get}
    }

  implicit val ctxIOAsk  = askFromHas[Ctx, Throwable, String]
  implicit val ctxIOErrAsk  = askFromHas[Ctx, Buzer, String]

  implicit def hellHandler = new Handle[CtxErrIO, Throwable] {
    def raise[E2 <: Throwable, A](e: E2): CtxErrIO[A] = ZIO.die(e)
    
    def applicative: Applicative[CtxErrIO] = implicitly
    
    def handleWith[A](fa: CtxErrIO[A])(f: Throwable => CtxErrIO[A]): CtxErrIO[A] =
      fa.catchAllDefect{x => f(x)}
  }

  val cosnole = new Cosnole[CtxErrIO] {
    def prntln(str: String): CtxErrIO[Unit] = ZIO.effectTotal(println(str))
  }
  
  implicit val funK: CtxIO ~> CtxErrIO = new (CtxIO ~> CtxErrIO) {
    def apply[A](fa: CtxIO[A]): CtxErrIO[A] = fa.catchAll(ZIO.die(_)) // let's just die if we got uncatched Throwable from suspicious region
  }

  val prg = new PrgFG[CtxIO, CtxErrIO](
    new FutaImpl[CtxIO],
    new PrettyLoopa[CtxErrIO],
    cosnole
  )

  import zio.console._

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    prg
      .things(Future.successful("1").pure[CtxIO])
      .provideCustomLayer(ZIO.succeed("1").toLayer)
      .tap(x => putStrLn("run : " + x))
      .fold(
          _ => ExitCode.failure,
          _ => ExitCode.success
      )
}