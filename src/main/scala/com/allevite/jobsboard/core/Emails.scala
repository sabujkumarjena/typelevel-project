package com.allevite.jobsboard.core

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.syntax.*
import com.allevite.jobsboard.config.EmailServiceConfig

import java.util.Properties
import javax.mail.internet.MimeMessage
import javax.mail.{Authenticator, Message, PasswordAuthentication, Session, Transport}

trait Emails[F[_]] {
  def sendEmail(to: String, subject: String, content: String): F[Unit]
  def sendPasswordRecoveryEmail(to: String, token: String): F[Unit]
}

class LiveEmails[F[_]: MonadCancelThrow] private (emailServiceConfig: EmailServiceConfig)
    extends Emails[F] {

  val host        = emailServiceConfig.host
  val port        = emailServiceConfig.port
  val user        = emailServiceConfig.user
  val pass        = emailServiceConfig.pass
  val frontendUrl = emailServiceConfig.frontendUrl

  // API
  override def sendEmail(to: String, subject: String, content: String): F[Unit] = {
    val messageResource = for {
      prop    <- propsResource
      auth    <- authenticatorResource
      session <- createSession(prop, auth)
      message <- createMessage(session)("sabuj@allevite.com", to, subject, content)
    } yield message

    messageResource.use(msg => (Transport.send(msg)).pure[F])
  }

  override def sendPasswordRecoveryEmail(to: String, token: String): F[Unit] = {
    val subject = "Alle Vite: Password Recovery"
    val content = s"""
    <div style="
    border: 1px solid black;
    padding: 20px;
    font-family: sans-serif;
    ">
    <h1> Alle Vite: Password recovery </h1>
    <p> Your password recovery token is $token </p>
    <p> Click <a href=$frontendUrl/login>here</p> to get back to the application
    <p> from Alle vite</p>
    """
      sendEmail(to, subject, content )
  }

  // private
  val propsResource: Resource[F, Properties] = {
    val prop = new Properties
    prop.put("mail.smtp.auth", true)
    prop.put("mail.smtp.starttls.enable", true)
    prop.put("mail.smtp.host", host)
    prop.put("mail.smtp.port", port)
    prop.put("mail.smtp.ssl.trust", host)
    Resource.pure(prop)
  }

  val authenticatorResource: Resource[F, Authenticator] = Resource.pure(
    new Authenticator {
      override protected def getPasswordAuthentication: PasswordAuthentication =
        new PasswordAuthentication(user, pass)
    }
  )

  def createSession(prop: Properties, auth: Authenticator): Resource[F, Session] =
    Resource.pure(Session.getInstance(prop, auth))

  def createMessage(
      session: Session
  )(from: String, to: String, subject: String, content: String): Resource[F, MimeMessage] = {
    val message = new MimeMessage(session)
    message.setFrom("sabuj.kumar.jena@gmail.com")
    message.setRecipients(Message.RecipientType.TO, "sabuj.kumar.jena@gmail.com")
    message.setSubject(subject)
    message.setContent(content, "text/html; charset=utf-8")
    Resource.pure(message)
  }
}
object LiveEmails {
  def apply[F[_]: MonadCancelThrow](emailServiceConfig: EmailServiceConfig): F[LiveEmails[F]] =
    new LiveEmails[F](emailServiceConfig).pure[F]
}
