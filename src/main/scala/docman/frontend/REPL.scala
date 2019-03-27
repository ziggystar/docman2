package docman.frontend

import java.io.{File, InputStream, OutputStream, PrintStream}

import cats.data.EitherT
import cats.effect.IO
import docman.core.DocAlgebra
import docman.engine.SideCarRO
import rx.lang.scala.{Observable, Observer, Subject, Subscription}
import Observable._

import scala.io.Source

object REPL{

  val backend = SideCarRO(Seq(new File("documents") -> true))

  def report[T](x: EitherT[IO,String,T]): Observable[String] =
    just(x.value.unsafeRunSync().fold[String](e => s"Error: $e", t => t.toString))

  def runRX(input: Observable[String], output: Observer[String]): Subscription = input.flatMap{
    case "list" => report(backend.getDocuments.map(_.map(_.toString).mkString("\n")))
    case x@_ => Observable.just(s"Unknown command: $x")
  }.subscribe(output)

  def main(args: Array[String]): Unit = {
    val ox = Subject[String]
    ox.onNext("Docman REPL")
    ox.subscribe(onNext = System.out.println, onError = e => System.err.println(e.toString))

    val ix = Subject[String]
    val subs = runRX(ix, ox)

    while(true) {
      ix.onNext(Source.stdin.bufferedReader().readLine())
    }
  }
}
