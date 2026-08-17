package ru.otus.module3.cats.catsconcurrencyhomework


import cats.effect.{Resource, Sync}
import cats.implicits.*
import Wallet.*

import java.io.IOException
import java.nio.file.{Files, Paths}
import java.nio.file.Files.*
import java.nio.file.Paths.*

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  private val path = Paths.get(s"$id.txt") // пусть совпадает с id

  private def createFile = {
    Files.createFile(path)
    Files.writeString(path, "0")
    BigDecimal(0)
  }

  private def write(amount: BigDecimal): F[Unit] = Sync[F].delay {
    Files.writeString(path, amount.toString)
  }.void

  def balance: F[BigDecimal] = Sync[F].delay {
    BigDecimal.apply(Files.readString(path))
  }.handleError {
    case _: IOException => createFile // ну а если у нас не получается получить число, то логично развалиться
  }

  def topup(amount: BigDecimal): F[Unit] = balance
    .flatMap(b =>
      write(b + amount)
    ).void


  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] =
    for {
      _balance <- balance
      operation <- if ((_balance - amount) < 0) Sync[F].pure(Left(Wallet.BalanceTooLow))
      else write(_balance - amount).map(Right(_))

    } yield operation

}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = Sync[F].delay(
    new FileWallet[F](id)
  )

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}