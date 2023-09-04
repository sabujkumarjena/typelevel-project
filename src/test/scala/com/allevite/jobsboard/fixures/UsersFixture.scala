package com.allevite.jobsboard.fixures

import com.allevite.jobsboard.domain.user.*
trait UsersFixture {
  val Sabuj = User(
    "sabuj@allevite.com",
    "allevite",
    Some("Sabuj"),
    Some("Jena"),
    Some("Alle Vite"),
    Role.ADMIN
  )

  val sabujEmail = Sabuj.email

  val Deepak = User(
    "deepak@allevite.com",
    "deepakrulez",
    Some("Deepak"),
    Some("Pradhan"),
    Some("Alle Vite"),
    Role.RECRUITER
  )

  val deepakEmail = Deepak.email

  val NewUser = User(
    "newuser@gmail.com",
    "simplepassword",
    Some("John"),
    Some("Doe"),
    Some("Some company"),
    Role.RECRUITER
  )

  val updatedDeepak = User(
    "deepak@allevite.com",
    "deepakrocks",
    Some("DEEPAK"),
    Some("PRADHAN"),
    Some("Adobe"),
    Role.RECRUITER
  )

}
