package com.allevite.jobsboard.whiteboard

import cats.effect.{IO, IOApp}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

object PasswordHashingPlayground extends IOApp.Simple {
  override def run: IO[Unit] =
    BCrypt.hashpw[IO]("allevite").flatMap(IO.println) >>
      BCrypt.hashpw[IO]("deepakrulez").flatMap(IO.println) >>
      BCrypt.hashpw[IO]("simplepassword").flatMap(IO.println) >>
      BCrypt.hashpw[IO]("deepakrocks").flatMap(IO.println)

//      BCrypt.checkpwBool[IO]("scalarocks", PasswordHash[BCrypt]("$2a$10$Z3gelWm1XWyH9mywh/Y6QujMNdqNLeb6gwqb5Nh9AC32wvGfxWOoq") )
//        .flatMap(IO.println)
}
