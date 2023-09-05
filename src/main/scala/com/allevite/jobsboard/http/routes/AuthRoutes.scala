package com.allevite.jobsboard.http.routes

import cats.*
import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger

import org.http4s.HttpRoutes
import org.http4s.server.Router

import com.allevite.jobsboard.http.validation.syntax.*
import com.allevite.jobsboard.core.*
class AuthRoutes[F[_]: Concurrent: Logger] private (auth: Auth[F]) extends HttpValidationDsl[F] {
  // POST /auth/login {LoginInfo} => 200 OK with Authorization: Bearer {jwt}
  private val loginRout: HttpRoutes[F] = HttpRoutes .of[F] {
    case POST -> Root / "login"=>
      Ok("TODO")
  }

  // POST /auth/users { NewUserInfo } => 201 Created
  private val createUserRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case POST -> Root / "users" =>
      Ok("TODO")
  }
  //PUT /auth/users/password {NewPasswordInfo}{ Authorization: Bearer {jwt}} => 200 OK
  private val changePasswordRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case PUT -> Root / "users" / "password" =>
      Ok("TODO")
  }
  //POST /auth/logout {Authorization: Bearer {jwt} } => 200 OK
  private val logoutRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case POST -> Root / "logout" =>
      Ok("TODO")
  }
  val routes = Router(
    "/jobs" -> (loginRout <+> createUserRoute <+> changePasswordRoute <+> logoutRoute)
  )
}

object AuthRoutes {
  def apply[F[_]: Concurrent: Logger](auth: Auth[F]) = new AuthRoutes[F](auth)
}
