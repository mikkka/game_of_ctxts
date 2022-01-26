# Game of contexts

Experimental repo to research context interaction and interop in mtl style

We should have 5 "modules" of logic with following "forms"
 - high performant computing with ability to defectize `cats.IO[A]` or `zio.UIO[A]`, `type `
 - reader computing with ability to defectize `Kleisli[IO, BazCtx, A]` or `zio.RIO[BazCtx, A]`
 - reader computing with ability to defectize `Kleisli[IO, GerCtx, A]` or `zio.RIO[GerCtx, A]`
 - reader computing with ability to defectize and custom errors tree FooErr `Kleisli[IO, R1, Either[FooErr, A]]` or `zio.ZIO[FooErr, R, A]`
 - reader computing with ability to defectize and custom errors tree BarErr `Kleisli[IO, R2, Either[BarErr, A]]` or `zio.ZIO[BarErr, R, A]`

In main program all errors should be handled, and context should be provided.

We have 2 contexts:
 - BarCtx
 - GerCtx

We have 2 errors families:
 - FooErr
 - BarErr
 - and fatal errors (somethig throwable)