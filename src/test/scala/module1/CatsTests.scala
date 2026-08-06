package module1

import org.scalatest.flatspec.AnyFlatSpec
import ru.otus.module1.collections2.{Cons, Nil, ToyList}
import ru.otus.module2.catsHomework.Tree.given
import ru.otus.module2.catsHomework.{Branch, Leaf, MonadError, Tree}
import ru.otus.module2.catsHomework.MonadError.given

import scala.util.{Failure, Success, Try}

class CatsTests extends AnyFlatSpec{

  "Tree functor" should "корректно обрабатывать лист" in {
    val tree = Leaf[Int](25)

    val newTree = TreeFunctor.map(tree)(v => v + 25)
    assert(newTree === Leaf[Int](50))
  }

  "Tree functor" should "корректно обрабатывать дерево" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))

    val treeResult = Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))

    val newTree = TreeFunctor.map(tree)(v => v + 1)
    assert(newTree === treeResult)
  }


  "MonadError[Try] pure" should "заворачивает значенияв маонаду" in {
    val expected = Try(25)

    val pure = TryMonadError.pure(25)

    assert(expected == pure)
  }

  "MonadError[Try] flatMap" should "должен работать корректно" in {
    val start = Try(25)
    val startTrow = Try[Int](throw RuntimeException("Test"))

    val tryMonad = TryMonadError

    assert(tryMonad.flatMap(start)(tr => Try(tr + 25)) == Try(50))
    assert(tryMonad.flatMap(startTrow)(tr => Try(tr + 25)).isFailure)
  }

  "MonadError[Try] raiseError" should "заворачивает ошибку" in {

    val raiseError = TryMonadError.raiseError(new RuntimeException("Test"))

    assert(raiseError.isFailure)
  }

  "MonadError[Try] handleErrorWith" should "обработает ошибку" in {

    val error = Failure(new RuntimeException("handleErrorWith"))

    val success = TryMonadError.handleErrorWith(error)(thr => Try(thr.getMessage))

    assert(error.isFailure)
    assert(success.isSuccess)
    assert(success.get.contains("handleErrorWith"))
  }

  "MonadError[Try] handleErrorWith" should "вернет оригинал" in {

    val success = Success("wtf")

    val newSuccess = TryMonadError.handleErrorWith(success)(smt => Try(smt.getMessage))

    assert(success.isSuccess)
    assert(newSuccess.isSuccess)
    assert(newSuccess == success)
  }

  "MonadError[Try] handleErrorWith" should "вернет ошибку если функция востоновления рухнет" in {

    val fail = Failure(new RuntimeException("handleErrorWith"))

    val wtf = TryMonadError.handleErrorWith(fail)(smt => Try(123 / 0))

    assert(fail.isFailure)
    assert(wtf.isFailure)
    assertThrows[ArithmeticException](wtf.get)
  }

  "MonadError[Try] handleError" should "обработает ошибку" in {

    val error = Failure(new RuntimeException("handleErrorWith"))

    val success = TryMonadError.handleError(error)(thr => thr.getMessage)

    assert(error.isFailure)
    assert(success.isSuccess)
    assert(success.get.contains("handleErrorWith"))
  }

  "MonadError[Try] handleError" should "вернет оригинал (ничего не сделает)" in {

    val success = Success("wtf")

    val newSuccess = TryMonadError.handleError(success)(smt => smt.getMessage)

    assert(success.isSuccess)
    assert(newSuccess.isSuccess)
    assert(newSuccess == success)
  }

  "MonadError[Try] ensure" should "вернет success если предикат положителен" in {

    val success = Success("wtf")

    val newSuccess = TryMonadError.ensure(success)(new RuntimeException("Not today"))(_.contains("wtf"))

    assert(newSuccess.isSuccess)
    assert(newSuccess == success)
  }

  "MonadError[Try] ensure" should "вернет fail если предикат ложен" in {

    val success = Success("wtf")

    val fail = TryMonadError.ensure(success)(new ArithmeticException("wtf"))(_.contains("Not today"))

    assert(fail.isFailure)
    assertThrows[ArithmeticException](fail.get)
  }

  // тут по честному бы поглядеть как ведет себя в котах, может и надо новый ексепшен вернуть
  "MonadError[Try] ensure" should "вернет оригинальный fail" in {

    val fail = Failure(new IllegalArgumentException("Give me the money"))

    val againFail = TryMonadError.ensure(fail)(new ArithmeticException("wtf"))(_ => true)

    assert(againFail.isFailure)
    assertThrows[IllegalArgumentException](fail.get)
  }



  "MonadError[Either] pure" should "заворачивает значенияв маонаду" in {
    val expected = Right(25)

    val pure = EitherMonadError.pure(25)

    assert(expected == pure)
  }

  "MonadError[Either] flatMap" should "должен работать корректно" in {
    val startR = Right(25)
    val startL = Left("wtf")

    assert(EitherMonadError.flatMap(startR)(tr => Right(tr + 25)) == Right(50))
    assert(EitherMonadError.flatMap(startL)(_ => Right(36)) == startL)
    assert(EitherMonadError.flatMap(startR)(_ => Left("oops")) == Left("oops"))
  }

  "MonadError[Either] raiseError" should "заворачивает ошибку" in {

    val raiseError = EitherMonadError.raiseError("RuntimeException")

    assert(raiseError.isLeft)
    assert(raiseError == Left("RuntimeException"))
  }

  "MonadError[Either] handleErrorWith" should "обработает ошибку" in {

    val left = Left("handleErrorWith")

    val right = EitherMonadError.handleErrorWith(left)(thr => Right(thr.length))


    assert(right.isRight)
    assert(right.contains("handleErrorWith".length))
  }

  "MonadError[Either] handleErrorWith" should "вернет оригинал" in {

    val right = Right("i'm right".toCharArray)

    val againRight = EitherMonadError.handleErrorWith(right)(smt => Left("oops"))

    assert(againRight.isRight)
    assert(againRight == right)
  }

  "MonadError[Either] handleErrorWith" should "вернет ошибку если функция востоновления рухнет" in {

    val left = Left("handleErrorWith")

    val wtf = EitherMonadError.handleErrorWith(left)(smt => Left("oops"))

    assert(wtf.isLeft)
    assert(wtf == Left("oops"))
  }

  "MonadError[Either] handleError" should "обработает ошибку" in {

    val left = Left("handleErrorWith")

    val right = EitherMonadError.handleError(left)(thr => thr)

    assert(right.isRight)
    assert(right.contains("handleErrorWith"))
  }

  "MonadError[Either] handleError" should "вернет оригинал (ничего не сделает)" in {

    val right = Right(67)

    val newRight = EitherMonadError.handleError(right)(smt => smt + 33)

    assert(newRight.isRight)
    assert(right == newRight)
  }

  "MonadError[Either] ensure" should "вернет right если предикат положителен" in {

    val right = Right(67)

    val newRight = EitherMonadError.ensure(right)("Not today")(_ > 0)

    assert(newRight.isRight)
    assert(newRight == right)
  }

  "MonadError[Either] ensure" should "вернет left если предикат ложен" in {

    val right = Right(67)

    val left = EitherMonadError.ensure(right)("wtf")(_ < 0)

    assert(left.isLeft)
    assert(left == Left("wtf"))
  }

  "MonadError[Either] ensure" should "вернет оригинальный left" in {

    val left = Left("Give me the money")

    val againLeft = EitherMonadError.ensure(left)("wtf")(_ => true)

    assert(againLeft.isLeft)
    // остался тотже
    assert(againLeft == left)
  }
}
