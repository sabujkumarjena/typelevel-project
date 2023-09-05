package com.allevite.jobsboard.core

import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import com.allevite.jobsboard.config.SecurityConfig
import com.allevite.jobsboard.domain.auth.*
import com.allevite.jobsboard.domain.security.*
import com.allevite.jobsboard.domain.user.*
import org.typelevel.log4cats.Logger
import tsec.authentication.{BackingStore, IdentityStore, JWTAuthenticator}
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

import scala.concurrent.duration.*
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
      users: Users[F]
  )(securityConfig: SecurityConfig): F[LiveAuth[F]] = {

    // 1. Identity store: String => OptionT[F, User]
    // identity store to retrieve users
    val idStore: IdentityStore[F, String, User] = (email: String) => OptionT(users.find(email))

    // 2. backing store for JWT token : BackingStore[F,id, JwtToken]
    val tokenStoreF = Ref.of[F, Map[SecureRandomId, JwtToken]](Map.empty).map { ref =>
      new BackingStore[F, SecureRandomId, JwtToken] {
        // mutable map - race condition
        // ref
        override def get(id: SecureRandomId): OptionT[F, JwtToken] =
          OptionT( /*F[JwtToken]*/ ref.get.map(_.get(id)))

        override def put(elem: JwtToken): F[JwtToken] =
          ref.modify(store => (store + (elem.id -> elem), elem))

        override def delete(id: SecureRandomId): F[Unit] = ref.modify(store => (store - id, ()))

        override def update(v: JwtToken): F[JwtToken] = put(v)

      }
    }
    // 3. key for hashing
    val keyF = HMACSHA256.buildKey[F](securityConfig.secret.getBytes("UTF-8")) // TODO move to config

    for {
      key        <- keyF
      tokenStore <- tokenStoreF
      // 4. jwt authenticator
      authenticator = JWTAuthenticator.backed.inBearerToken(
        // expiry of tokens, max idle, idStore, key
        expiryDuration = securityConfig.jwtExpiryDuration,  // expiration of tokens
        maxIdle = None,          // max idle time (optional)
        identityStore = idStore, // identity store
        tokenStore = tokenStore,
        signingKey = key // hash key
      )
    } yield
    // 5. live auth
    new LiveAuth[F](users, authenticator)
  }
}
