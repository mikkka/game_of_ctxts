package com.example.gofc

import cats.effect.IO
import cats.data.Kleisli
import zio.UIO
import zio.RIO
import zio.ZIO
import cats.mtl.Raise
import cats.data.EitherT

object types {
  sealed trait BazCtx {
    def baz: String
  }
  sealed trait GerCtx {
    def ger: String
  }

  trait FooErr
  trait BarErr

  object ce {
    type TSK[A] = IO[A] // can be bombed with throwable

    type BazTSK[A] = Kleisli[IO, BazCtx, A] // can be bombed with throwable
    type GerTSK[A] = Kleisli[IO, GerCtx, A] // can be bombed with throwable

    type BazFooTSK[A] = EitherT[Kleisli[IO, BazCtx, *], FooErr, A] // can be bombed with throwable or FooErr
    type GerBarTSK[A] = EitherT[Kleisli[IO, GerCtx, *], BarErr, A] // can be bombed with throwable or BarErr
  }
  object zio {
    type TSK[A] = UIO[A] // can be defected with throwable

    type BazTSK[A] = RIO[BazCtx, A] // can be defected with throwable
    type GerTSK[A] = RIO[GerCtx, A] // can be defected with throwable

    type BazFooTSK[A] = ZIO[BazCtx, FooErr, A] // can be defected with throwable or errorized with FooErr
    type GerBarTSK[A] = ZIO[GerCtx, BarErr, A] // can be defected with throwable or errorized with FooErr
  }

  // type Defect[F[_]] = Raise[F, Throwable]
}