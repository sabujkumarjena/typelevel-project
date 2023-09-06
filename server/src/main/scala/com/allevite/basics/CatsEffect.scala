package com.allevite.basics

import cats.{Defer, MonadError}
import cats.effect.kernel.Fiber
import cats.effect.{
  Concurrent,
  Deferred,
  GenSpawn,
  IO,
  IOApp,
  MonadCancel,
  Ref,
  Resource,
  Spawn,
  Sync,
  Temporal
}

import java.io.{File, PrintWriter}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Random
object CatsEffect extends IOApp.Simple {
  /*
  describe computations as values
   */
  // IO = data structure describing arbitary computations ( including side effects)
  val firstIO: IO[Int] = IO.pure(45)
  val delayedIO: IO[Int] = IO {
    // complex code
    println(" Doing some work..")
    45
  }

  def evaluateIO[A](io: IO[A]): Unit = {
    import cats.effect.unsafe.implicits.global // platform
    val mol = io.unsafeRunSync()
    println(s"The result of the effect is : $mol")
  }

  // ransformations
  // map + flatMap
  // old style of standard scala apps
  //  def main(args: Array[String]): Unit = {
  //    evaluateIO(delayedIO)
  //  }

  // CE apps have a "run" method returning an IO, which will internally be evaluated in a main function

  // raise/"catch" error
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException(" aproper failure"))
  val dealWithIt = aFailure.handleErrorWith { case _: RuntimeException =>
    IO(println(" I am still here, no worries"))
  }
  // fibers = "lightweight threads"
  val delayedPrint = IO.sleep(1.second) >> IO(println(Random.nextInt(100)))
  val manyPrints = for {
    fib1 <- delayedPrint.start
    fib2 <- delayedPrint.start
    _    <- fib1.join
    _    <- fib2.join
  } yield ()

  val cancelledFiber = for {
    fib <- delayedPrint.onCancel(IO(println("I am cancelled!"))).start
    _   <- IO.sleep(500.millis) >> IO(println("Cancelling fiber")) >> fib.cancel
    _   <- fib.join
  } yield ()

  // uncancellation
  val ignoreCancellation = for {
    fib <- IO.uncancelable(_ => delayedPrint.onCancel(IO(println("I am cancelled!")))).start
    _   <- IO.sleep(500.millis) >> IO(println("Cancelling fiber")) >> fib.cancel
    _   <- fib.join
  } yield ()

  // resources
  val readingResource = Resource.make(
    IO(
      scala.io.Source.fromFile(
        "/Users/sabuj/Downloads/typelevel-project/src/main/scala/com/allevite/basics/CatsEffect.scala"
      )
    )
  )(source => IO(println(" closing source")) >> IO(source.close()))

  // compose resources
  val copiedFileResource = Resource.make(
    IO(
      new PrintWriter(
        new File("/Users/sabuj/Downloads/typelevel-project/src/main/resources/dumpedFile.scala")
      )
    )
  )(writer => IO(println("closing duplicated file")) >> IO(writer.close()))

  val compositeResource = for {
    source <- readingResource
    dest   <- copiedFileResource
  } yield (source, dest)

  val copyEffect = compositeResource.use { case (source, dest) =>
    IO(source.getLines().foreach(dest.println))
  }

  // abstract kinds of computations

  // MonadCancel = cancelable computation
  trait MyMonadCancel[F[_], E] extends MonadError[F, E] {
    trait CancellationFlagResetter {
      def apply[A](fa: F[A]): F[A] // with the cancellation flag reset // make cancelable
    }
    def canceled: F[Unit]
    def uncancelable[A](poll: CancellationFlagResetter => F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]
  val uncancelableIO = monadCancelIO.uncancelable(_ => IO(45)) // same as IO.uncancelable(...)

  // Spawn = ability to create fibers
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, E, A]] // creates a fiber
    // never, cede, racePair
  }
  trait MySpawn[F[_]] extends GenSpawn[F, Throwable]

  val spawnIO = Spawn[IO]
  val fiber   = spawnIO.start(delayedPrint) // creates a fiber, same as delayedPrint.start

  // Concurrent = cuncurrency primitives ()atomic references + promise (defer))
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]] // atomically updating(reading and writing) a ref
    def deferred[A]: F[Deferred[F, A]]
  } // using ref and defeered all other concurrency primitives like mutex, semaphore, barrier can be derived

  // Temporal = ability to suspend computation for a given time
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

//Sync = ability to suspend synchronous arbitrary expressions in an effect
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](expression: => A): F[A]
    def blocking[A](exression: => A): F[A] // runs on a dedicated thread pool
  }

//Async = ability to suspend asynchronous computation( i.e on other thread pools) into an effect manged by CE

  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
  }

  override def run: IO[Unit] = {
    // IO.unit
    // cancelledFiber
    copyEffect
  }
}
