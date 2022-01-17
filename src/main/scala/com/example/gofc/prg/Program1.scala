package com.example.gofc.prg

import com.example.gofc.prg.tsk._
import com.example.gofc.prg.baz._
import com.example.gofc.prg.ger._
import com.example.gofc.prg.baz_foo._
import com.example.gofc.prg.ger_bar._
import cats.Monad
import cats.syntax.all._


trait Program[F[_]]{
  def meeting(theme: String): F[String]
}

class ProgramImpl[F[_]: Monad](
  mrWhite : MrWhite[F],
  mrRed   : MrRed[F],
  mrGreen : MrGreen[F],
  mrBrown : MrBrown[F],
  mrOrange: MrOrange[F]
)extends Program[F] {
  def meeting(theme: String): F[String] = for {
    white <- mrWhite.hello()
    _     <- mrWhite.puke()

    red   <- mrRed.hello()
    _     <- mrRed.puke()
    
    green <- mrGreen.hello()
    _     <- mrGreen.puke()

    brown <- mrBrown.hello()
    _     <- mrBrown.puke()

    orange <- mrOrange.hello()
    _      <- mrOrange.puke()
  } yield (white + red + green + brown + orange)
}
