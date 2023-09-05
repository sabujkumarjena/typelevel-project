package com.allevite.jobsboard.fixures

import com.allevite.jobsboard.domain.user.*

/*
allevite => $2a$10$hCkLO1p6W4dAmFtgpCifquMszyQSkLAW94oYgOcv3uMK7.mY8rCfu
deepakrulez =>  $2a$10$cptmdsaJ2aTw4l2nzxOKDeUPysgwDqpd9Jir6BkcUiIKBc0VIcJfC
simplepassword => $2a$10$7G0yBADs6tlB68T70e8v6.Jpf7NwWlYwjdos6p9D8rvKvp3NZ.SVu
deepakrocks =>  $2a$10$Z5HPEKqr1cffRnxs3HqiiumtcFXcFohNqUEMWHEkd5MWJPYcLIWmK
 */
trait UserFixture {
  val Sabuj = User(
    "sabuj@allevite.com",
    "$2a$10$hCkLO1p6W4dAmFtgpCifquMszyQSkLAW94oYgOcv3uMK7.mY8rCfu",
    Some("Sabuj"),
    Some("Jena"),
    Some("Alle Vite"),
    Role.ADMIN
  )

  val sabujEmail    = Sabuj.email
  val sabujPassword = "allevite"

  val Deepak = User(
    "deepak@allevite.com",
    "$2a$10$cptmdsaJ2aTw4l2nzxOKDeUPysgwDqpd9Jir6BkcUiIKBc0VIcJfC",
    Some("Deepak"),
    Some("Pradhan"),
    Some("Alle Vite"),
    Role.RECRUITER
  )

  val deepakEmail    = Deepak.email
  val deepakPassword = "deepakrulez"
  val NewUser = User(
    "newuser@gmail.com",
    "$2a$10$7G0yBADs6tlB68T70e8v6.Jpf7NwWlYwjdos6p9D8rvKvp3NZ.SVu",
    Some("John"),
    Some("Doe"),
    Some("Some company"),
    Role.RECRUITER
  )

  val updatedDeepak = User(
    "deepak@allevite.com",
    "$2a$10$Z5HPEKqr1cffRnxs3HqiiumtcFXcFohNqUEMWHEkd5MWJPYcLIWmK",
    Some("DEEPAK"),
    Some("PRADHAN"),
    Some("Adobe"),
    Role.RECRUITER
  )

  val NewUserSabuj = NewUserInfo(
    sabujEmail,
    sabujPassword,
    Some("Sabuj"),
    Some("Jena"),
    Some("Alle Vite")
  )

  val NewUserDeepak = NewUserInfo(
    deepakEmail,
    deepakPassword,
    Some("Deepak"),
    Some("Pradhan"),
    Some("Alle Vite")
  )

}
