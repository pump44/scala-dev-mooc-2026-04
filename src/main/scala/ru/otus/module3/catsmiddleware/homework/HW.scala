package ru.otus.module3.catsmiddleware.homework

import cats.Functor
import cats.data.{Kleisli, OptionT}
import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.{IO, IOApp, Resource, Temporal}
import cats.effect.kernel.{Concurrent, Ref}
import com.comcast.ip4s.{Host, Port}
import io.circe.Encoder.AsObject.importedAsObjectEncoder
import io.circe.generic.auto.deriveEncoder
import io.circe.{Decoder, Json, ParsingFailure}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.{HttpRoutes, dsl}
import org.http4s.{Method, Request, Status, Uri}
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import ru.otus.module3.catsmiddleware.{CircyJson, Restfull}
import fs2.{Chunk, Pure, Stream}

import scala.concurrent.duration.FiniteDuration.given
import scala.util.Try
import scala.concurrent.duration.*

/*
Описание/Пошаговая инструкция выполнения домашнего задания:
Обновите свой форк учебного репозитория Otus

Задача:

HTTP эндпоинт /counter
Возвращающий JSON в виде {"counter": 1}, где число увеличивается с каждым запросом, поступающим на этот эндпоинт

HTTP эндпоинт /slow/:chunk/:total/:time выдающий искусственно медленный ответ, имитируя сервер под нагрузкой
:chunk, :total и :time - переменные, которые пользователь эндпоинта может заменить числами, например запрос по адресу
/show/10/1024/5 будет выдавать body кусками по 10 байт, каждые 5 секунд пока не не достигнет 1024 байт.
Содержимое потока на усмотрение учащегося - может быть повторяющийся символ, может быть локальный файл.
Неверные значения переменных (строки или отрицательные числа в переменных) должны приводить к ответу Bad Request
 */

object HW {

  type Counter[F[_]] = Ref[F, Int]

  import Count._

  def serviceCounter(counter: Counter[IO]): HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root =>
        for {
          value <- counter.modify(v => (v + 1, v + 1))
          resp <- Ok(Count(value))
        } yield resp
    }
  }

  def serviceSlowly: HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root / IntVar(chunk) / IntVar(total) / IntVar(time) =>
        Ok(slow[IO](chunk, total, time))
    }
  }

  def router(counter: Counter[IO]): HttpRoutes[IO] = Router(
    "/counter" -> serviceCounter(counter),
    "/slow" -> parameterMiddleware(serviceSlowly)
  )

  val server: Resource[IO, Server] = for {
    counter <- Resource.eval(Ref.of[IO, Int](0))
    s <- EmberServerBuilder
      .default[IO]
      .withHost(Host.fromString("localhost").get)
      .withPort(Port.fromInt(8081).get)
      .withHttpApp(router(counter).orNotFound)
      .build
  } yield s


  def parameterMiddleware(routes: HttpRoutes[IO]): HttpRoutes[IO] = Kleisli {
    req =>
      req.uri.path.segments.tail.forall{s => s.encoded.toIntOption.nonEmpty} match {
        case true => routes(req)
        case _ => OptionT.liftF(BadRequest("Только положительные числа"))
      }
  }

  def slow[F[_] : Temporal](chunk: Int, total: Int, time: Int): Stream[F, Chunk[Byte]] = {
    val stream: Stream[F, Chunk[Byte]] = Stream
      .constant[F, Byte](0)
      .take(total)
      .chunkN(chunk, allowFewer = true)

      stream
      .spaced(time.seconds)
  }
}

object mainServer extends IOApp.Simple {
  def run: IO[Unit] = {
    HW.server.use(_ => IO.never)
  }
}


case class Count(counter: Int)

object Count {
  implicit val decoderUser: Decoder[Count] = Decoder.derived
}