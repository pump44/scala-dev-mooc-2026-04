package ru.otus.module1

import scala.util.Random

object homework5 {

  sealed trait Ball

  case object Black extends Ball
  case object White extends Ball

  case class Bucket() {
    val balls: List[Ball] = Random.shuffle(List(White, White, White, Black, Black, Black))


    def attempt(
                 check: ((Ball, Ball)) => Boolean
               ): Boolean = {
      val first = balls.head
      // не хотелось дробить и делайть склейку, решил просто перемешать хвост
      // а идеи без склейки с иммутабельной труктурой другой не пришло
      val second = Random.shuffle(balls.tail).head
      check((first, second))
    }
  }

  case class GroupExperiment(count: Int)(check: ((Ball, Ball)) => Boolean) {
    def make(): String = {
      val buckets = List.iterate(Bucket(), count) { _ => Bucket() }

      val p = (buckets.map(s => s.attempt(check)).count {
        _ == true
      } / count.toDouble) * 100

      s"${p.toInt} %"
    }
  }

  @main
  def start(): Unit = {

    // не очень понятно какой эксперимент нужно преоверить по заданию, но так
    // можно проверять любые передавая функцию провеки
    // Ну почти любые)
    def atLeastOneWhite(pull: (Ball, Ball)): Boolean = pull._1 == White || pull._2 == White
    def firstBlackSecondWhite(pull: (Ball, Ball)): Boolean = pull == (Black, White)

    println(GroupExperiment(10000)(atLeastOneWhite).make())
    println(GroupExperiment(10000)(firstBlackSecondWhite).make())
    println(
      GroupExperiment(10000){(_, second) => second == White}.make()
    )
  }

}
