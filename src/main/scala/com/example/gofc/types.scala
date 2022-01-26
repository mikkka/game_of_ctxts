package com.example.gofc

import cats.effect.IO
import cats.data.Kleisli
import zio.UIO
import zio.RIO
import zio.ZIO
import cats.mtl.Raise
import cats.data.EitherT
import cats.Monad

object types {
  trait BazCtx {
    def baz: String
  }
  trait GerCtx {
    def ger: String
  }

  trait FooErr {
    def cause: String
  }

  trait BarErr {
    def cause: String
  }

  object ce {
    type TSK[A] = IO[A] // can be bombed with throwable

    type BazTSK[A] = Kleisli[IO, BazCtx, A] // can be bombed with throwable
    type GerTSK[A] = Kleisli[IO, GerCtx, A] // can be bombed with throwable

    type BazFooTSK[A] = EitherT[BazTSK, FooErr, A] // can be bombed with throwable or FooErr
    type GerBarTSK[A] = EitherT[GerTSK, BarErr, A] // can be bombed with throwable or BarErr
  }
  object zi {
    import zio.blocking._
    import zio.clock._
    import zio.Has

    type TSK[A] = RIO[Clock with Blocking, A] // can be defected with throwable

    type BazTSK[A] = RIO[Has[BazCtx], A] // can be defected with throwable
    type GerTSK[A] = RIO[Has[GerCtx], A] // can be defected with throwable

    type BazFooTSK[A] = ZIO[Has[BazCtx], FooErr, A] // can be defected with throwable or errorized with FooErr
    type GerBarTSK[A] = ZIO[Has[GerCtx], BarErr, A] // can be defected with throwable or errorized with FooErr
  }

  // type Defect[F[_]] = Raise[F, Throwable]
}