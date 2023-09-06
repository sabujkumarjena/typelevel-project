package com.allevite.jobsboard.config

import cats.MonadThrow
import pureconfig.{ConfigReader, ConfigSource}
import cats.implicits.*
import pureconfig.error.ConfigReaderException

import scala.reflect.ClassTag

object syntax {
  extension (source: ConfigSource)
    def loadF[F[_], A](using reader: ConfigReader[A], f: MonadThrow[F], tag: ClassTag[A]): F[A] =
      f.pure(source.load[A]).flatMap {
        case Left(error) => f.raiseError[A](ConfigReaderException[A](error))
        case Right(value) => f.pure(value)
      }
}
