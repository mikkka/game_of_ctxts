package com.example.gofc

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cats.syntax.all._

object IOAppa extends IOApp {
  def run(args: List[String]): IO[ExitCode] = 
    Console[IO].println("\nhello world\n").as(ExitCode.Success)
}

