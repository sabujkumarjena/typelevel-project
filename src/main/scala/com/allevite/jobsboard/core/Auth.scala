package com.allevite.jobsboard.core

import cats.effect.*
import cats.implicits.*
import com.allevite.jobsboard.domain.auth.*
import com.allevite.jobsboard.domain.security.*
import com.allevite.jobsboard.domain.user.*
import org.typelevel.log4cats.Logger
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash
trait Auth[F[_]] {
  def login(email: String, password: String): F[Option[JwtToken]]
  def signUp(newUserInfo: NewUserInfo): F[Option[User]]
  def changePassword(
      email: String,
      newPasswordInfo: NewPasswordInfo
  ): F[Either[String, Option[User]]]
  def delete(email: String): F[Boolean]
  def authenticator: Authenticator[F] = ???
  // TODO password recovery via email
}

class LiveAuth[F[_]: Async: Logger] private (
    users: Users[F],
    override val authenticator: Authenticator[F]
) extends Auth[F] {
  override def login(email: String, password: String): F[Option[JwtToken]] =
    for {
      // find the user in the DB -> return None if no user
      maybeUser <- users.find(email)
      // check password
      maybeValidatedUser <- maybeUser.filterA(user =>
        BCrypt
          .checkpwBool[F](
            password,
            PasswordHash[BCrypt](user.hashedPassword)
          )
      )
      //  return a new token if password matches
      maybeJwtToken <- maybeValidatedUser.traverse(user => authenticator.create(user.email))
    } yield maybeJwtToken

  override def signUp(newUserInfo: NewUserInfo): F[Option[User]] = for {
    // find the user in the db, if found => None
    maybeUser <- users.find(newUserInfo.email)
    result <- maybeUser match {
      case Some(_) => None.pure[F]
      case None =>
        for {
          // hash the new password
          hashedPassword <- BCrypt.hashpw[F](newUserInfo.password)
          user <- User(
            newUserInfo.email,
            hashedPassword,
            firstName = newUserInfo.firstName,
            lastName = newUserInfo.lastName,
            company = newUserInfo.company,
            role = Role.RECRUITER
          ).pure[F]
          // create a new user in db
          _ <- users.create(user)
        } yield Some(user)
    }

  } yield result

  override def changePassword(
      email: String,
      newPasswordInfo: NewPasswordInfo
  ): F[Either[String, Option[User]]] = {

    def updateUser(user: User, newPassword: String): F[Option[User]] =
      for {
        hashedPassword <- BCrypt.hashpw[F](newPasswordInfo.newPassword)
        updatedUser    <- users.update(user.copy(hashedPassword = hashedPassword))
      } yield updatedUser

    def checkAndUpdate(
        user: User,
        oldPassword: String,
        newPassword: String
    ): F[Either[String, Option[User]]] = for {
      passCheck <- BCrypt
        .checkpwBool[F](
          newPasswordInfo.oldPassword,
          PasswordHash[BCrypt](user.hashedPassword)
        )
      updateResult <-
        if (passCheck) updateUser(user, newPassword).map(Right(_))
        else Left("Invalid password").pure[F]
    } yield updateResult

    users.find(email).flatMap { // find user
      case None => Right(None).pure[F]
      case Some(user) =>
        val NewPasswordInfo(oldPassword, newPassword) = newPasswordInfo
        checkAndUpdate(user, oldPassword, newPassword)

    }
  }

  override def delete(email: String): F[Boolean] = users.delete(email)

}

object LiveAuth {
  def apply[F[_]: Async: Logger](
      users: Users[F],
      authenticator: Authenticator[F]
  ): F[LiveAuth[F]] =
    new LiveAuth[F](users, authenticator).pure[F]
}
