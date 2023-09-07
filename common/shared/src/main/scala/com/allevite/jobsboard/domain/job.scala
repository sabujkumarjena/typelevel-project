package com.allevite.jobsboard.domain

import java.util.UUID

object job {
  case class Job(
      id: UUID,
      date: Long,
      ownerEmail: String,
      jobInfo: JobInfo,
      active: Boolean = false
  )
  case class JobInfo(
      company: String,
      title: String,
      description: String,
      externalUrl: String,
      remote: Boolean,
      location: String,
      salaryHi: Option[Int],
      salaryLo: Option[Int],
      currency: Option[String],
      country: Option[String],
      tags: Option[List[String]],
      image: Option[String],
      seniority: Option[String],
      other: Option[String]
  )

  object JobInfo {
    val empty: JobInfo =
      JobInfo("", "", "", "", false, "", None, None, None, None, None, None, None, None)

    def minimal(
        company: String,
        title: String,
        description: String,
        externalUrl: String,
        remote: Boolean,
        location: String
    ): JobInfo = JobInfo(
      company,
      title,
      description,
      externalUrl,
      remote,
      location,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  final case class JobFilter (
      companies: List[String] = Nil,
      locations: List[String] = Nil,
      countries: List[String] = Nil,
      seniorities: List[String] = Nil,
      tags: List[String] = Nil,
      maxSalary: Option[Int] = None,
      remote: Boolean = false
                             )
}