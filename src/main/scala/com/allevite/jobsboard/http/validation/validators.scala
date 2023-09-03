package com.allevite.jobsboard.http.validation

import com.allevite.jobsboard.domain.job.*
import cats.*
import cats.data.*
import cats.data.Validated.*
import cats.implicits.*

import java.net.URL
import scala.util.{Failure, Success, Try}
object validators {
  sealed trait ValidationFailure(val errorMessage: String)
  case class EmptyField(fieldName: String) extends ValidationFailure(s"'$fieldName' is empty")
  case class InvalidUrl(fieldName: String) extends ValidationFailure(s"'$fieldName' is invalid url")
  // empty field, invalid URL, invalid email

  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]
  trait Validator[A] {
    def validate(value: A): ValidationResult[A]
  }

  def validateRequired[A](field: A, fieldName: String)(
      required: A => Boolean
  ): ValidationResult[A] = if (required(field)) field.validNel
  else EmptyField(fieldName).invalidNel

  def validateUrl(field: String, fieldName: String): ValidationResult[String] =
    Try(URL(field).toURI()) match {
      case Success(_) => field.validNel
      case Failure(_) => InvalidUrl(fieldName).invalidNel
    }
  given jobInfoValidator: Validator[JobInfo] = (jobInfo: JobInfo) => {
    val JobInfo(
      company,     // should not be empty
      title,       // should not be empty
      description, // should not be empty
      externalUrl, // should be a valid URL
      remote,
      location, // should not be empty
      salaryHi,
      salaryLo,
      currency,
      country,
      tags,
      image,
      seniority,
      other
    ) = jobInfo

    val validCompany     = validateRequired(company, "company")(_.nonEmpty)
    val validTitle       = validateRequired(title, "title")(_.nonEmpty)
    val validDescription = validateRequired(description, "description")(_.nonEmpty)
    val validExternalUrl = validateUrl(externalUrl, "externalUrl")
    val validLocation    = validateRequired(location, "location")(_.nonEmpty)

    (
      validCompany,
      validTitle,
      validDescription,
      validExternalUrl,
      remote.validNel,
      validLocation,
      salaryHi.validNel,
      salaryLo.validNel,
      currency.validNel,
      country.validNel,
      tags.validNel,
      image.validNel,
      seniority.validNel,
      other.validNel
    ).mapN(JobInfo.apply) //ValidatedNel[ValidationFailure, JobInfo]


  }

}
