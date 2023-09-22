package com.allevite.jobsboard.fixures
import cats.syntax.all.*
import com.allevite.jobsboard.domain.job.*

import java.util.UUID

import com.allevite.jobsboard.domain.job
trait JobFixture {

  val NotFoundJobUuid = UUID.fromString("6ea79557-3112-4c84-a8f5-1d1e2c300948")

  val AwesomeJobUuid = UUID.fromString("843df718-ec6e-4d49-9289-f799c0f40064")

  val AwesomeJob = Job(
    AwesomeJobUuid,
    1659186086L,
    "sabuj@allevite.com",
    JobInfo(
      "Awesome Company",
      "Tech Lead",
      "An awesome job in Berlin",
      "https://allevite.com/awesomejob",
      false,
      "Berlin",
      2000.some,
      3000.some,
      "EUR".some,
      "Germany".some,
      Some(List("scala", "scala-3", "cats")),
      None,
      "Senior".some,
      None
    ),
    active = true
  )

  val InvalidJob = Job(
    null,
    42L,
    "nothing@gmail.com",
    JobInfo.empty
  )

  val UpdatedAwesomeJob = Job(
    AwesomeJobUuid,
    1659186086L,
    "sabuj@allevite.com",
    JobInfo(
      "Awesome Company (Spain Branch)",
      "Engineering Manager",
      "An awesome job in Barcelona",
      "http://www.awesome.com",
      false,
      "Barcelona",
      2200.some,
      3200.some,
      "USD".some,
      "Spain".some,
      Some(List("scala", "scala-3", "zio")),
      "http://www.awesome.com/logo.png".some,
      "Highest".some,
      "Some additional info".some
    ),
    active = true
  )

  val AlleviteNewJob = JobInfo(
    "Allevite",
    "Technical Author",
    "For the glory of the Allevite",
    "https://allevite.com/",
    true,
    "From remote",
    2000.some,
    3500.some,
    "EUR".some,
    "Romania".some,
    Some(List("scala", "scala-3", "cats", "akka", "spark", "flink", "zio")),
    None,
    "High".some,
    None
  )

  val AlleviteobWithNotFoundId = AwesomeJob.copy(id = NotFoundJobUuid)

  val AnotherAwesomeJobUuid = UUID.fromString("19a941d0-aa19-477b-9ab0-a7033ae65c2b")
  val AnotherAwesomeJob     = AwesomeJob.copy(id = AnotherAwesomeJobUuid)

  val AlleviteAwesomeJob =
    AwesomeJob.copy(jobInfo = AwesomeJob.jobInfo.copy(company = "RockTheJvm"))

  val NewJobUuid = UUID.fromString("efcd2a64-4463-453a-ada8-b1bae1db4377")
  val AwesomeNewJob = JobInfo(
    "Awesome Company",
    "Tech Lead",
    "An awesome job in Berlin",
    "example.com",
    false,
    "Berlin",
    2000.some,
    3000.some,
    "EUR".some,
    "Germany".some,
    Some(List("scala", "scala-3", "cats")),
    None,
    "High".some,
    None
  )
}
