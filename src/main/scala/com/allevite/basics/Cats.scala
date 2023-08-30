package com.allevite.basics

object Cats {
  /*
    type classes
    - Applicative
    - Functor
    - FlatMap
    - Monad
    - ApplicativeError/MonadError
   */
  // functor - "mappable" structure
  trait MyFunctor[F[_]] {
    def map[A, B](container: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.* // contains given instances of Functor[List] *** AUTOMATICALLY IMPORTED NOT REQUIRED EXPLICITLY**
  val listFunctor = Functor[List]
  val mappedList  = listFunctor.map(List(1, 2, 3))(_ + 1)

  // generalizable "mappable" APIs
  def increament[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.* // EM map
  def increament_v2[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    container.map(_ + 1)

  // applicative - pure, wrap existing values into "wrapper" values
  trait MyApplicative[F[_]] extends Functor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList     = applicativeList.pure(42)
  import cats.syntax.applicative.* // EM for pure
  val aSimpleList_v2 = 42.pure[List]

  // flatMap
  trait MyFlatMap[F[_]] extends Functor[F] {
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList    = FlatMap[List]
  val flatMappedList = flatMapList.flatMap(List(1, 2, 3))(x => List(x, x + 1))
  import cats.syntax.flatMap.* // EM for flatMap
  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))
  def crossProduct_v2[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a,b)

  // Monad - applicative + flatMap

  trait MyMonad[F[_]] extends Applicative[F] with FlatMap[F]

  import cats.Monad
  val monadList = Monad[List]

  def crossProduct_v3[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  // applicative-error - computations that can fail
  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val applicativeEither = ApplicativeError[ErrorOr, String]
  val desiredValue: ErrorOr[Int] = applicativeEither.pure(42)
  val failedValue: ErrorOr[Int] = applicativeEither.raiseError("Something bad happened")
  import cats.syntax.applicativeError.* // EMs for pure and raiseError
  val failedValue_v2: ErrorOr[Int] = "Something bad happened".raiseError

  //monad-error
  trait MyMonadError[F[_], E] extends  ApplicativeError[F, E] with Monad[F]
  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]
  val desiredValueMonad: ErrorOr[Int] = monadErrorEither.pure(42)
  val failedValueMonad: ErrorOr[Int] = monadErrorEither.raiseError("Something bad happened")
  import cats.syntax.monadError.* //EMS for pure and raiseError
  val failedValueMonad_v2: ErrorOr[Int] = "Something bad happened".raiseError
  def main(args: Array[String]): Unit = {}
}
