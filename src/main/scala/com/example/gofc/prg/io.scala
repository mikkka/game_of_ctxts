package com.example.gofc.prg

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
import scala.concurrent.Future
import cats._
import cats.syntax.all._
import cats.instances.all._
import com.example.gofc.types._
import com.example.gofc.types.ce._
import com.example.gofc.prg.tsk.MrWhiteImpl
import com.example.gofc.prg.baz.MrRedImpl
import com.example.gofc.prg.ger.MrGreenImpl
import com.example.gofc.prg.baz_foo.MrBrownImpl
import com.example.gofc.prg.ger_bar.MrOrange
import com.example.gofc.prg.ger_bar.MrOrangeImpl

object IOAppa extends IOApp {
  type Ctx = (BazCtx,GerCtx)
  type Err = Either[FooErr,BarErr]
  type KleisliX[A] = Kleisli[IO, Ctx, A]
  type X[A] = EitherT[KleisliX, Err, A]

  implicit def hellRaiser[F[_]](implicit ME: MonadError[F, Throwable]) = new Raise[F, Throwable] {
    def functor: Functor[F] = ME
    def raise[E2 <: Throwable, A](e: E2): F[A] = ME.raiseError(e)
  }

  val bazToXK: BazTSK ~> KleisliX = new (BazTSK ~> KleisliX) {
    def apply[A](fa: BazTSK[A]): KleisliX[A] = 
      fa.compose { ctx: Ctx => ctx._1.pure[IO] }
  }

  val gerToXK: GerTSK ~> KleisliX = new (GerTSK ~> KleisliX) {
    def apply[A](fa: GerTSK[A]): KleisliX[A] = 
      fa.compose { ctx: Ctx => ctx._2.pure[IO] }
  }
  
  val funK1: TSK ~> X = Kleisli.liftK.andThen[X](EitherT.liftK[KleisliX, Err])

  val funK2: BazTSK ~> X = new (BazTSK ~> X) {
    def apply[A](fa: BazTSK[A]): X[A] = 
      EitherT.liftF[KleisliX, Err, A](bazToXK(fa))
  }

  val funK3: GerTSK ~> X = new (GerTSK ~> X) {
    def apply[A](fa: GerTSK[A]): X[A] = 
      EitherT.liftF[KleisliX, Err, A](gerToXK(fa))
  }
  val funK4: BazFooTSK ~> X = new (BazFooTSK ~> X) {
    def apply[A](fa: BazFooTSK[A]): X[A] = {
      fa.leftMap(_.asLeft[BarErr]).mapK(bazToXK)
    }
  }
  val funK5: GerBarTSK ~> X = new (GerBarTSK ~> X) {
    def apply[A](fa: GerBarTSK[A]): X[A] =
      fa.leftMap(_.asRight[FooErr]).mapK(gerToXK)
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

  def run(args: List[String]): IO[ExitCode] = {
    val ctx = (
      new BazCtx {
        def baz: String = "kekus"
      }, 
      new GerCtx {
        def ger: String = "barabekus"
      }
    )
    prg.meeting("robbery").value.run(ctx)
    .flatTap(x => Console[IO].println("run : " + x))
    .map(_.fold(
      _ => ExitCode.Error,
      _ => ExitCode.Success
    ))
  }
}