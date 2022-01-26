package com.example.gofc.prg

import cats.Applicative
import cats.MonadError
import cats._
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.effect.std
import cats.mtl.Ask
import cats.mtl.Handle
import cats.mtl.Raise
import cats.syntax.all._
import com.example.gofc.prg.baz.MrRedImpl
import com.example.gofc.prg.baz_foo.MrBrownImpl
import com.example.gofc.prg.ger.MrGreenImpl
import com.example.gofc.prg.ger_bar.MrOrange
import com.example.gofc.prg.ger_bar.MrOrangeImpl
import com.example.gofc.prg.tsk.MrWhiteImpl
import com.example.gofc.types._
import com.example.gofc.types.zi._
import izumi.reflect.Tag
import zio.ExitCode
import zio.Has
import zio.URIO
import zio.ZEnv
import zio.ZIO
import zio.ZLayer
import zio.blocking._
import zio.clock._
import zio.interop.catz._
import zio.interop.catz.mtl._

import scala.concurrent.Future

object ask {
  def fromHas[R, E, A](
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
}

object hhandler {
  def handlerFor[R,E] = new Handle[ZIO[R,E,*], Throwable] {
    def raise[E2 <: Throwable, A](e: E2): ZIO[R,E,A] = ZIO.die(e)
    def applicative: Applicative[ZIO[R,E,*]] = implicitly
    def handleWith[A](fa: ZIO[R,E,A])(f: Throwable => ZIO[R,E,A]): ZIO[R,E,A] =
      fa.catchAllDefect(f)
  }
}

object ZIOAppa extends CatsApp {
  type Ctx0 = Clock with Blocking
  type Ctx  = Clock with Blocking with Has[BazCtx] with Has[GerCtx]
  type Err  = Either[FooErr,BarErr]

  type X[A] = ZIO[Ctx, Err, A]

  implicit val ask1  = ask.fromHas[Has[BazCtx], Throwable, BazCtx]
  implicit val ask2  = ask.fromHas[Has[GerCtx], Throwable, GerCtx]
  implicit val ask3  = ask.fromHas[Has[BazCtx], FooErr, BazCtx]
  implicit val ask4  = ask.fromHas[Has[GerCtx], BarErr, GerCtx]
  implicit val hh1   = hhandler.handlerFor[Has[BazCtx], FooErr]
  implicit val hh2   = hhandler.handlerFor[Has[GerCtx], BarErr]

  val funK1: TSK ~> X = new (TSK ~> X) {
    def apply[A](fa: TSK[A]): X[A] = fa.catchAll(ZIO.die(_))
  }
  val funK2: BazTSK ~> X = new (BazTSK ~> X) {
    def apply[A](fa: BazTSK[A]): X[A] = fa.catchAll(ZIO.die(_))
  }
  val funK3: GerTSK ~> X = new (GerTSK ~> X) {
    def apply[A](fa: GerTSK[A]): X[A] = fa.catchAll(ZIO.die(_))
  }
  val funK4: BazFooTSK ~> X = new (BazFooTSK ~> X) {
    def apply[A](fa: BazFooTSK[A]): X[A] = fa.catchAll(x => ZIO.fail(x.asLeft))
  }
  val funK5: GerBarTSK ~> X = new (GerBarTSK ~> X) {
    def apply[A](fa: GerBarTSK[A]): X[A] = fa.catchAll(x => ZIO.fail(x.asRight))
  }
  
  val prg = new ProgramImpl[X, TSK, BazTSK, GerTSK, BazFooTSK, GerBarTSK](
    new MrWhiteImpl[TSK],
    new MrRedImpl[BazTSK],
    new MrGreenImpl[GerTSK],
    new MrBrownImpl[BazFooTSK],
    new MrOrangeImpl[GerBarTSK]
  )(
    funK1,
    funK2,
    funK3,
    funK4,
    funK5
  ) 

  import zio.console._

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val ctx1 = ZIO.succeed(
      new BazCtx {
        def baz: String = "kekus"
      } 
    ).toLayer

    val ctx2 = ZIO.succeed(
      new GerCtx {
        def ger: String = "barabekus"
      }
    ).toLayer

    prg.meeting("robbery").provideCustomLayer(ctx1 ++ ctx2)
      .tap(x => putStrLn("run : " + x))
      .fold(
          _ => ExitCode.failure,
          _ => ExitCode.success
      )
  }
}