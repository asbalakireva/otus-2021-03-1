package homework

import cats.effect._
import cats.syntax.all._
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.{HttpRoutes}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._
import com.comcast.ip4s.{Host, Port}
import org.http4s.server.Router

import scala.concurrent.duration._

case class Counter(counter: Int)

object Restfull  {

  def serviceOne(ref: Ref[IO, Int]) : HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "counter" =>
        for {
          x <- ref.modify(x => (x + 1, x))
          resp <- Ok(Counter(x).asJson)
        } yield resp
    }

  val serviceTwo: HttpRoutes[IO] = {
    def soSlow(chunk: Int, total: Int, time: Int): Stream[IO, String] = {
      (Stream.awakeEvery[IO](time.second) *>
        Stream.constant(1)).map(_.toString).repeat.take(total).chunkN(chunk).map(v => v.toList.fold("")(_ + _) + "\n")
    }

    HttpRoutes.of[IO] {
      case GET -> Root / "slow" / chunk / total / time =>
        try {
          val ch = chunk.toInt
          val to = total.toInt
          val ti = time.toInt

          if (ch <= 0 || to <= 0 || ti <= 0) BadRequest("Значения должны быть больше 0")
          else Ok(soSlow(ch, to, ti))
        }
        catch {
          case _: Throwable => BadRequest("Неверное число")
        }
    }
  }


  def router(ref: Ref[IO, Int]) = Router("/" -> (serviceOne(ref) <+> serviceTwo))

  val server = for {
    ref <-  Resource.eval(Ref.of[IO, Int](1))
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router(ref).orNotFound).build
  } yield s
}
  object mainServer extends IOApp.Simple {
    def run(): IO[Unit] = {
      Restfull.server.use(_ => IO.never)
    }
}


