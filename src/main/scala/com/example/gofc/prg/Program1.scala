package com.example.gofc.prg

import cats.Monad
import cats._
import cats.effect.kernel.Async
import cats.mtl.Ask
import cats.mtl.Handle
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
  F[_] : Monad : Async,
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
  fg: F ~> G,
  gh: G ~> H,
  hi: H ~> I,
  ij: I ~> J,
  jx: J ~> X
) extends Program[X] {
  def meeting(theme: String): X[List[(String, String)]] = {
    val white = for {
      white1 <- mrWhite.hello(theme)
      white2 <- mrWhite.puke()
    } yield (white1, white2)

    val red = for {
      red1 <- mrRed.hello()
      red2 <- mrRed.puke()
    } yield (red1, red2)

    val green = for {
      green1 <- mrGreen.hello()
      green2 <- mrGreen.puke()
    } yield (green1, green2)

    val brown = for {
      brown1 <- mrBrown.hello()
      brown2 <- mrBrown.puke()
    } yield (brown1, brown2)

    val orange = for {
      orange1 <- mrOrange.hello()
      orange2 <- mrOrange.puke()
    } yield (orange1, orange2)

    Nil.pure[X] >>= { x => 
      jx(
        ij(
          hi(
            gh(
              fg(white).map(_ :: x) >>= {x => red.map(_ :: x)}
            ) >>= {x => green.map(_ :: x)}
          ) >>= {x => brown.map(_ :: x)}
        ) >>= {x => orange.map(_ :: x)}
      )
    }
  }
}