package ru.otus.module2

import cats.Functor

import scala.util.{Failure, Success, Try}

package object catsHomework {

  /**
   * Простое бинарное дерево
   * @tparam A
   */
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */

  object Tree {
    
    given TreeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f)) // ну тут конечно не хвостовая
        case Leaf(value) => Leaf(f(value))
      }
    }
  }



  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
  }


  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */

  object MonadError {

    given TryMonadError: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
      def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa match {
        case Failure(exception) => Failure(exception)
        case Success(value) => f(value)
      }

      def pure[A](v: A): Try[A] = Try(v)

      // Поднимаем ошибку в контекст `F`:
      def raiseError[A](e: Throwable): Try[A] = Failure(e)

      // Обработка ошибки, потенциальное восстановление:
      def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa match {
        case Failure(exception) => f(exception)
        case s => s
      }

      // Обработка ошибок, восстановление от них:
      def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa match {
        case Failure(exception) => Try(f(exception))
        case s => s
      }

      // Test an instance of `F`,
      // failing if the predicate is not satisfied:
      def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] = fa match {
        case s @ Success(value) if f(value) => s
        case Success(value) => Failure(e)
        case f => f
      }
    }


    /**
     * Напишите instance MonadError для Either,
     * где в качестве типа ошибки будет String
     */

    // Фиксируем стрингу через тайп
    type LeftStringEither [R] = Either[String, R]

    given EitherMonadError: MonadError[LeftStringEither, String] with  {

      override def raiseError[A](e: String): LeftStringEither[A] = Left(e)

      override def handleErrorWith[A](fa: LeftStringEither[A])(f: String => LeftStringEither[A]): LeftStringEither[A] = fa match {
        case Left(value) => f(value)
        case r => r
      }

      // можно использовать handleErrorWith ???
      override def handleError[A](fa: LeftStringEither[A])(f: String => A): LeftStringEither[A] = fa match {
        case Left(value) => Right(f(value))
        case r => r
      }

      override def ensure[A](fa: LeftStringEither[A])(e: String)(f: A => Boolean): LeftStringEither[A] = fa match {
        case r @ Right(value) if f(value) => r
        case Right(value) => Left(e)
        case _ => fa
      }

      override def flatMap[A, B](fa: LeftStringEither[A])(f: A => LeftStringEither[B]): LeftStringEither[B] = fa match {
        case Right(value) => f(value)
        case Left(value) => Left(value)
      }

      override def pure[A](v: A): LeftStringEither[A] = Right(v)
    }

  }



}
