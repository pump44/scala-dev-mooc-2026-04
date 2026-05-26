package ru.otus.module1.collectionsHW

object collectionsTask {
  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /** Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
    * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
    * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
    * Пример:
    * capitalizeIgnoringASCII(List("Lorem", "ipsum","dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
    * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
    * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
    * HINT: Тут удобно использовать collect и zipWithIndex
    *
    * *
    */
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    text.zipWithIndex.collect{
      case (el, 0) => el
      case (el, _) if isASCIIString(el) => el.toUpperCase()
      case (el, _) => el.toLowerCase
    }
  }

  def capitalizeIgnoringASCII2(text: List[String]): List[String] = {
    text match {
      case head :: tail =>
        tail.foldLeft(head :: Nil) {
          case (acc, el) if isASCIIString(el) => acc :+ el.toUpperCase()
          case (acc, el) => acc :+ el.toLowerCase()
        }
      case Nil => Nil
    }
  }

  /** Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
    * В тексте встречаются числа от 0 до 9
    *
    * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
    *
    * HINT: Для всех возможных комбинаций чисел стоит использовать Map
    * *
    */
  def numbersToNumericString(text: String): String = {
    // ну с ключами String проще реализовать
    val translator = Map(
      "1" -> "one",
      "2" -> "two",
      "3" -> "three",
      "4" -> "four",
      "5" -> "five",
      "6" -> "six",
      "7" -> "seven",
      "8" -> "eight",
      "9" -> "nine",
    )

    text
      .split(" ")
      .map(str => translator.getOrElse(str, str))
      .mkString(" ")
  }

  def numbersToNumericString2(text: String): String = {
    // ну можно и инты
    val translator = Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine",
    )

    val translate: String => String = (mbNumber: String) => mbNumber.toIntOption match {
      case Some(v) => translator.getOrElse(v, mbNumber)
      case None => mbNumber
    }

    text
      .split(" ")
      .map(str => translate(str))
      .mkString(" ")

    // тк у нас числа до 9 можно и не бить массив, а пойти по Char
    // но что если число часть слова, пароль какой
//    text
//      .flatMap { char => translator.getOrElse(char.asDigit, char.toString)}
  }

  /** У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
    * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии, что записи уникальные и не имеют повторений
    * HINT: Set
    * HINT2: Iterable стоит изменить
    * *
    */

  case class Auto(mark: String, model: String)

  /** Хотим узнать какие машины можно обслужить учитывая этих двух дилеров.
    * Реализуйте метод, который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
    */
  def intersectionAuto(
      dealerOne: Iterable[Auto],
      dealerTwo: Iterable[Auto]
  ): Iterable[Auto] = {
    Set[Auto]().empty ++ dealerTwo ++ dealerTwo
  }

  /** Хотим узнать какие машины обслуживается в первом дилерском центре, но не обслуживаются во втором.
    * Реализуйте метод, который примет две коллекции (два источника)
    * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
    */
  def filterAllLeftDealerAutoWithoutRight(
      dealerOne: Iterable[Auto],
      dealerTwo: Iterable[Auto]
  ): Iterable[Auto] = {
    dealerOne.toSet.&~(dealerTwo.toSet)

    //dealerOne.filterNot(car => dealerTwo.exists(_ == car)).toSet
  }
}
