package ru.otus.module1

import ru.otus.module1.concurrency.{MyThread, getRatesLocation1, getRatesLocation2, printRunningTime}

import scala.util.{Failure, Success}


object App {
  def main(args: Array[String]): Unit = {
    println(s"Hello world from: " +
      s"${Thread.currentThread().getName}")

//    val t1 = new Thread {
//      override def run(): Unit = {
//        Thread.sleep(1000)
//        println(s"Hello from ${Thread.currentThread().getName}")
//      }
//    }
//    val t2 = new MyThread
//    t1.start()
//    t1.join()
//    t2.start()

    def action = {
      val f1 = getRatesLocation1
      val f2 = getRatesLocation2

      val r2: concurrency.ToyFuture[Int] = for{
        i1 <- f1
        i2 <- f2
      } yield i1 + i2

      r2.onComplete(println)

//      val r: Unit = f1.onComplete {
//        case Failure(exception) =>
//          println(exception.getMessage)
//        case Success(i1) =>
//          f2.onComplete {
//            case Failure(exception) =>
//              println(exception.getMessage)
//            case Success(i2) =>
//              println(s"Result: ${i1 + i2}")
//          }
//      }
    }
    printRunningTime(action)
  }
}
