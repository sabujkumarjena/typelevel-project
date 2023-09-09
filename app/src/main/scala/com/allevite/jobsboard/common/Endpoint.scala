package com.allevite.jobsboard.common

import tyrian.*
import tyrian.http.*
import cats.effect.IO
import io.circe.Encoder
import io.circe.syntax.*
import com.allevite.jobsboard.core.*
trait Endpoint[M] {
  val location: String
  val method: Method
  val onResponse: Response => M
  val onError: HttpError => M

  def call[A: Encoder](payload: A): Cmd[IO, M] =
    internalCall(payload, None)

  def call(): Cmd[IO, M] = internalCall(None)

  def callAuthorized[A: Encoder](payload: A): Cmd[IO, M] =
    internalCall(payload, Session.getUserToken())

  def callAuthorized(): Cmd[IO, M] =
    internalCall(Session.getUserToken())
  // private api
  private def internalCall[A: Encoder](payload: A, authorization: Option[String]): Cmd[IO, M] = {
    Http.send(
      Request(
        url = location,
        method = method,
        headers = authorization.map(token => Header("Authorization", token)).toList,
        body = Body.json(payload.asJson.toString),
        timeout = Request.DefaultTimeOut,
        withCredentials = false
      ),
      Decoder[M](onResponse, onError)
    )
  }

  private def internalCall(authorization: Option[String]): Cmd[IO, M] = {
    Http.send(
      Request(
        url = location,
        method = method,
        headers = authorization.map(token => Header("Authorization", token)).toList,
        body = Body.Empty,
        timeout = Request.DefaultTimeOut,
        withCredentials = false
      ),
      Decoder[M](onResponse, onError)
    )
  }

}
