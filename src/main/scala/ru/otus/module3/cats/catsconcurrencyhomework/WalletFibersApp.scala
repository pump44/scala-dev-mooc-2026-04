package ru.otus.module3.cats.catsconcurrencyhomework

import cats.effect.{IO, IOApp, Sync, Temporal}
import cats.implicits.*
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.Duration

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {


  // правильно ли от Temporal ? или надо выбрать другой интерфейс
  extension [F[_]: Temporal] (wallet: Wallet[F]) {
    def topupLoop(interval: Duration): F[Nothing] =
      wallet.topup(100) >> Temporal[F].sleep(interval) >> topupLoop(interval)
  }

  private def show(wallets: Wallet[IO]*): IO[Unit] =
    wallets.traverse(_.balance)
      .flatMap(values => IO.println(s"${values.mkString(", ")}")) >>
      IO.sleep(1.seconds)
      >> show(wallets: _*)


  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      f1 <- wallet1.topupLoop(100.millis).start
      f2 <- wallet2.topupLoop(500.millis).start
      f3 <- wallet3.topupLoop(2000.millis).start
      bal <- show(wallet1, wallet2, wallet3).start
      _ <- IO.readLine
      _ <- f1.cancel
      _ <- f2.cancel
      _ <- f3.cancel
      _ <- bal.cancel
    } yield ()

}