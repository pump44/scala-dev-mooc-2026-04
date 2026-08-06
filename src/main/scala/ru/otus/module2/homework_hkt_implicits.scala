package ru.otus.module2

import scala.util.Try

object homework_hkt_implicits {

  /*
  *  Реализовать общий метод tupleF
  */

  trait Mapper[F[_], A] {
    def map[B](f: A => B): F[B]

    def flatMap[B](f: A => F[B]): F[B]
  }

  object Mapper {
  // нам все равно надо реализовывать под каждый тип
    given [A]: Conversion[Option[A], Mapper[Option, A]] with
      override def apply(x: Option[A]): Mapper[Option, A] = new Mapper[Option, A] {
        override def map[B](f: A => B): Option[B] = x.map(f)

        override def flatMap[B](f: A => Option[B]): Option[B] = x.flatMap(f)
      }

    given [A]: Conversion[List[A], Mapper[List, A]] with
      override def apply(x: List[A]): Mapper[List, A] = new Mapper[List, A] {
        override def map[B](f: A => B): List[B] = x.map(f)

        override def flatMap[B](f: A => List[B]): List[B] = x.flatMap(f)
      }


    given [A]: Conversion[Try[A], Mapper[Try, A]] with
      override def apply(x: Try[A]): Mapper[Try, A] = new Mapper[Try, A] {
      override def map[B](f: A => B): Try[B] = x.map(f)

      override def flatMap[B](f: A => Try[B]): Try[B] = x.flatMap(f)
    }
  }

  def tupleF[F[_], A, B](fa: Mapper[F, A], fb: Mapper[F, B]): F[(A, B)] =
    fa.flatMap { a => fb.map((a, _)) }

  @main
  def runTypleF(): Unit = {
    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)

    val try1 = Try(1 / 4)
    val try2 = Try("try")
    

    val r1 = println(tupleF(optA, optB))
    val r2 = println(tupleF(list1, list2))
    val r3 = println(tupleF(try1, try2))
  }


}


object homework_hkt_implicits_2 {

  // такой вариант кажется еще более монструозным
  trait Mapper[F[_]]  {
      def map[A, B](fa: F[A])(f: A => B): F[B]

      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  
  object Mapper {

    // тут бы конструктор, но как-то не получилось )

    given Mapper[List] = new Mapper[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

    given Mapper[Option] = new Mapper[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }
  }

  def tupleF[F[_], A, B](fa: F[A], fb: F[B])(using m: Mapper[F]): F[(A, B)] = {
    m.flatMap(fa){ a => m.map(fb)((a, _)) }

  }

  @main
  def runTypleFTwo(): Unit = {
    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)

    val r1 = println(tupleF(optA, optB))
    val r2 = println(tupleF(list1, list2))
  }

}