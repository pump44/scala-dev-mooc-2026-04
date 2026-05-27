package ru.otus.module1

import ru.otus.module1.utils.NameableThreads

import java.util.concurrent.{ExecutorService, Executors}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object concurrency {

  class MyThread extends Thread {
    override def run(): Unit =
      println(s"Hello from ${Thread.currentThread().getName}")
  }

  def printRunningTime(f: => Unit): Unit = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
  }

  def getRatesLocation1: ToyFuture[Int] = ToyFuture {
    Thread.sleep(1000)
    println("Location 1")
    10
  }(executors.pool1)

  def getRatesLocation2: ToyFuture[Int] = ToyFuture {
    Thread.sleep(2000)
    println("Location 2")
    20
  }(executors.pool1)

  def async(f: => Unit): Thread = new Thread {
    override def run(): Unit = f
  }

  def async2[A](f: => A): A = {
    var v: A = null.asInstanceOf[A]
    val t = new Thread {
      override def run(): Unit =
        v = f
    }
    t.start()
    v
  }

  class ToyFuture[T]private(v: Option[() => T], ex: ExecutorService) {
    private var r: Try[T] = null.asInstanceOf[Try[T]]
    private var isCompleted: Boolean = false
    private val q = mutable.Queue[Try[T] => _]()

    def onComplete[U](f: Try[T] => U): Unit =
      if (isCompleted) f(r)
      else q.enqueue(f)

    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = {
      val result = ToyFuture.pending[B](ex)
      this onComplete {
        case Success(value) =>
          f(value).onComplete{ b =>
            result.complete(b)
          }
        case Failure(exception) =>
          result.complete(Try(throw new Exception(exception.getMessage)))
      }
      result
    }

    def map[B](f: T => B): ToyFuture[B] = flatMap(v => ToyFuture[B](f(v))(ex))


    def complete(result: Try[T]): Unit =
      if(!isCompleted){
        r = result
        isCompleted = true
        while (q.nonEmpty) {
          q.dequeue()(r)
        }
      }


    private def start() = {
      val t = new Runnable {
        override def run(): Unit = {
          r = Try(v.get())
          isCompleted = true
          while (q.nonEmpty) {
            q.dequeue()(r)
          }
        }
      }
      ex.execute(t)
    }
  }

  object ToyFuture {
    def apply[T](v: => T)(ex: ExecutorService): ToyFuture[T] = {
      val f = new ToyFuture[T](Some(() => v), ex)
      f.start()
      f
    }

    def pending[T](ex: ExecutorService): ToyFuture[T] =
      new ToyFuture[T](None, ex)
  }









}

object executors {
  val pool1: ExecutorService =
    Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
  val pool2: ExecutorService =
    Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
  val pool3: ExecutorService =
    Executors.newWorkStealingPool(4)
  val pool4: ExecutorService =
    Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}


