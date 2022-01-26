package com.example.gofc.prg

import cats.Monad
import cats._
import cats.effect.kernel.Async
import cats.mtl.Ask
import cats.mtl.Handle
import cats.mtl.syntax.all._
import cats.syntax.all._
import com.example.gofc.prg.baz._
import com.example.gofc.prg.baz_foo._
import com.example.gofc.prg.ger._
import com.example.gofc.prg.ger_bar._
import com.example.gofc.prg.tsk._
import com.example.gofc.types._

import scala.concurrent.Future

trait Program[X[_]]{
  def meeting(theme: String): X[List[(String, String)]]
}

class ProgramImpl[
  X[_] : Monad,
  F[_] : Monad : Async : Handle[*[_], Throwable],
  G[_] : Monad : Ask[*[_], BazCtx] : Handle[*[_], Throwable],
  H[_] : Monad : Ask[*[_], GerCtx] : Handle[*[_], Throwable],
  I[_] : Monad : Ask[*[_], BazCtx] : Handle[*[_], FooErr] : Handle[*[_], Throwable],
  J[_] : Monad : Ask[*[_], GerCtx] : Handle[*[_], BarErr] : Handle[*[_], Throwable],
](
  mrWhite : MrWhite[F],
  mrRed   : MrRed[G],
  mrGreen : MrGreen[H],
  mrBrown : MrBrown[I],
  mrOrange: MrOrange[J]
)(
  fg: F ~> X,
  gh: G ~> X,
  hi: H ~> X,
  ij: I ~> X,
  jx: J ~> X
) extends Program[X] {
  def meeting(theme: String): X[List[(String, String)]] = {
    val white = for {
      white1 <- mrWhite.hello(theme)
      white2 <- mrWhite.puke().handle{e: Throwable => e.getMessage()}
    } yield (white1, white2)

    val red = for {
      red1 <- mrRed.hello()
      red2 <- mrRed.puke().handle{e: Throwable => e.getMessage()}
    } yield (red1, red2)

    val green = for {
      green1 <- mrGreen.hello()
      green2 <- mrGreen.puke().handle{e: Throwable => e.getMessage()}
    } yield (green1, green2)

    val brown = for {
      brown1 <- mrBrown.hello().handle{e: FooErr => e.cause}
      brown2 <- mrBrown.puke().handle{e: Throwable => e.getMessage()}
    } yield (brown1, brown2)

    val orange = for {
      orange1 <- mrOrange.hello().handle{e: BarErr => e.cause}
      orange2 <- mrOrange.puke().handle{e: Throwable => e.getMessage()}
    } yield (orange1, orange2)

    (fg(white), gh(red), hi(green), ij(brown), jx(orange)).mapN((_1, _2, _3, _4, _5) => 
      List(_1, _2, _3, _4, _5)
    )
  }
}