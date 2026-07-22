package ru.otus.module3

import zio.*

import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {

  /** 1.
    * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
    * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
    */

  val checkInput: String => IO[String, Int] = maybeNumber =>
    for {
      number <- ZIO.attempt(maybeNumber.toInt).orElseFail("Вы ввели не число!")
      _ <- ZIO.when(number < 1 || number > 3) {
        ZIO.fail("Необходимо число от 1 до 3")
      }
    } yield number

  val checkLuck: (Either[String, Int], Int) => Task[String] = (guess, random) =>
    ZIO.attempt {
      guess match {
        case Left(message) => message
        case Right(value) =>
          if (value == random) "Вы угадали!"
          else s"Вы не угадали, я загадал $random"
      }
    }

  lazy val guessProgram: Task[Unit] = for {
    randomNumber <- Random.nextIntBetween(1, 4)
    _ <- Console.printLine("Угадайте число от 1 до 3")
    userInput <- Console.readLine
    userGuess <- checkInput(
      userInput
    ).either // можно такто и упасть, но так типо не ломаемся, а вывод делаем один раз
    check <- checkLuck(userGuess, randomNumber)
    _ <- Console.printLine(check)
  } yield ()

  /** 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
    */

  // вот только не значение ли нам нужно? Но мы же про зио, вроде вернуть сам эффект норм
  def doWhile[R, E, A](ef: ZIO[R, E, A], _while: A => Boolean): ZIO[R, E, A] = {
    ef.flatMap(a => if (_while(a)) ZIO.succeed(a) else doWhile(ef, _while))
  }

  def chekWhile = doWhile(
    Random.nextIntBetween(1, 4),
    (value) => { println(value); value == 2 }
  )

  /** 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
    * и выведет его в консоль
    * Используйте эффект "Configuration.config" из пакета config
    */

  val default = config.AppConfig("localhost", "8080")

  def loadConfigOrDefault: IO[Config.Error, config.AppConfig] = for {
    config <- config.Configuration.config
      .catchAllCause(_ => ZIO.succeed(default))
      .tap(Console.printLine(_).orDie)
  } yield config

  /** 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
    * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
    * на изменение этих сигнатур
    */

  /** 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
    * Используйте сервис zio Random
    */
  lazy val eff: UIO[Int] = for {
    number <- Random.nextIntBetween(0, 11)
    _ <- Clock.sleep(1 seconds)
  } yield number

  /** 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
    */
  lazy val effects: Seq[UIO[Int]] = List.fill(10)(eff)

  /** 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
    * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
    * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
    */

  lazy val app = for {
    _sum <- zioConcurrency.printEffectRunningTime(
      ZIO.collectAll(effects).map(_.sum)
    )
    _ <- Console.printLine(s"Сумма: ${_sum}")
  } yield ()

  /** 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
    */

  lazy val appSpeedUp = for {
    _sum <- zioConcurrency.printEffectRunningTime(
      ZIO.collectAllPar(effects).map(_.sum)
    )
    _ <- Console.printLine(s"Сумма: ${_sum}")
  } yield ()

  /** 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
    * можно было использовать аналогично zio.Console.printLine например
    */

  trait EffectTime {
    def print[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A]
  }
  
  class EffectTimeImpl(console: Console, clock: Clock) extends EffectTime {
    private def currentTime: UIO[Long] = clock.currentTime(TimeUnit.SECONDS)

    override def print[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = for {
      start <- currentTime
      r <- zio
      end <- currentTime
      _ <- console.printLine(s"Running time: ${end - start}").orDie
    } yield r
  }

  object EffectTime {
    val live: ZLayer[Console with Clock, Nothing, EffectTime] =
      ZLayer(
        for {
          console <- ZIO.service[Console]
          clock <- ZIO.service[Clock]
        } yield new EffectTimeImpl(console, clock)
      )

    // а вот тут вопрос, это не будет создавать сервис на каждый вызов
    // т.е. если у нас 10 вызовов принта, или сервис будет тащить одни из условно глобального скопа?
    def print[R, E, A](zio: ZIO[R, E, A]): ZIO[EffectTime with R, E, A] =
      ZIO.serviceWithZIO[EffectTime](_.print(zio))
  }

  /** 6.
    * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
    */

  lazy val appWithTimeLogg =
    EffectTime.print(ZIO.collectAll(effects).map(_.sum))

  /** Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  val appLayer: ZLayer[Any, Nothing, EffectTime] = ZLayer.make[EffectTime](
    EffectTime.live,
    ZLayer.succeed(Console.ConsoleLive),
    ZLayer.succeed(Clock.ClockLive)
  )

  lazy val runApp: UIO[Int] = appWithTimeLogg.provideLayer(appLayer)

}
