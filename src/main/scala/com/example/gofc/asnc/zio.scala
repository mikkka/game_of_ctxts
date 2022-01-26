package com.example.gofc.asnc

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
import cats.mtl.Ask
import izumi.reflect.Tag

object ZIOAppa extends CatsApp {

  // can't do this, because Async for ZIO[_ <: non throwable,_,_] is missing!
  // type CtxErrIO[A] = ZIO[String, Buzer, A]
  // val ctxIOErrAsync = implicitly[Async[CtxErrIO]]

  type Ctx = Clock with Blocking with Has[String]
  type CtxIO[A] = ZIO[Ctx, Throwable, A]
  val ctxIOAsync = implicitly[Async[CtxIO]]
  
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

  implicit def hellHandler = new Handle[CtxErrIO, Throwable] {
    def raise[E2 <: Throwable, A](e: E2): CtxErrIO[A] = EitherT.right(ZIO.die(e))
    
    def applicative: Applicative[CtxErrIO] = implicitly
    
    def handleWith[A](fa: CtxErrIO[A])(f: Throwable => CtxErrIO[A]): CtxErrIO[A] =
      EitherT[CtxIO, Buzer, A](fa.value.catchAllDefect{x => f(x).value})
  }

  type CtxErrIO[A] = EitherT[CtxIO, Buzer, A]
  val ctxErrIOAsync = implicitly[Async[CtxErrIO]]
  val ctxErrIOAskHs = implicitly[Ask[CtxErrIO, String]]

  val cosnole = new Cosnole[CtxErrIO] {
    def prntln(str: String): CtxErrIO[Unit] = 
      EitherT.right(ZIO.effectTotal(println(str)))
  }

  val prg = new PrgF[CtxErrIO](
    new FutaImpl[CtxErrIO],
    new PrettyLoopa[CtxErrIO],
    cosnole
  )

  import zio.console._

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val ctx = ZIO.succeed("1").toLayer
    val res = prg
      .things(Future.successful("101").pure[CtxErrIO]).value

    res
      .provideCustomLayer(ctx)
      .tap(x => putStrLn("run : " + x))
      .fold(
          _ => ExitCode.failure,
          _ => ExitCode.success
      )
  }
}