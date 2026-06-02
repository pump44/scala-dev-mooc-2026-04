package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.task

import scala.concurrent
import scala.concurrent.impl.Promise
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правовой результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */

  // жрет лимиты нещадно
  def fullSequence1[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    val fs: Future[List[Try[A]]] = Future.sequence(
    futures
      .map(f => f.transform(Try(_)))
    )

    val suc: Future[List[A]] = fs.map(_.collect{case Success(v) => v})
    val fail: Future[List[Throwable]] = fs.map(_.collect{case Failure(ex) => ex})

    suc.zip(fail)
  }

  // это лучше, но не идеально, надо как можно меньше запуска фьюч
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    futures.foldRight(Future.successful((List[A](), List[Throwable]()))) { (f, acc) =>
      f.flatMap {
        v => acc.map((lf, lt) => (v +: lf, lt))
      }
        .recoverWith {
          case t: Throwable => acc.map((lf, lt) => (lf, t +: lt))
      }
    }
  }


  // тоже пожирает токены
  def fullSequence2[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    val fs: Future[List[A]] = Future.sequence(
      futures
    )

    fs.map{list =>
      list.foldRight((List[A](), List[Throwable]())){
        case (v: A, (lf, lt)) => (v +: lf, lt)
        case (e: Throwable, (lf, lt)) => (lf, e +: lt)
      }
    }

    }

}
