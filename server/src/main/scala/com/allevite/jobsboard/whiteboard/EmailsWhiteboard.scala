package com.allevite.jobsboard.whiteboard

import cats.effect.{IO, IOApp}

import java.util.Properties
import javax.mail.internet.MimeMessage
import javax.mail.{Authenticator, Message, PasswordAuthentication, Session, Transport}
import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.config.*
object EmailsEffectWhiteboard extends IOApp.Simple {
  override def run: IO[Unit] = for {
    emails <- LiveEmails[IO](
      EmailServiceConfig(
        host = "smtp.ethereal.email",
        port = 587,
        user = "sherman.herzog@ethereal.email",
        pass = "5mXG4maqwsd6CCp2S4",
        frontendUrl = "htps://google.com"
      )
    )
    _ <- emails.sendPasswordRecoveryEmail("someone@allevite.com", "ALLEVITE")
  } yield ()
}
object EmailsWhiteboard {
  def main(args: Array[String]): Unit = {
    // configs
    // user, pass, host, port
    /*
  Host	smtp.ethereal.email
  Port	587
  Security	STARTTLS
  Username	sherman.herzog@ethereal.email
  Password	5mXG4maqwsd6CCp2S4
     */
    val host        = "smtp.ethereal.email"
    val port        = 587
    val user        = "sherman.herzog@ethereal.email"
    val pass        = "5mXG4maqwsd6CCp2S4"
    val token       = "ABCD1234"
    val frontendUrl = "htps://google.com"

    // properties file
    /*
    mail.smtp.auth = true
    mail.smtp.starttls.enable = true
    mail.smtp.host = host
    mail.smtp.port = port
    mail.smtp.ssl.trust = host

     */
    val prop = new Properties
    prop.put("mail.smtp.auth", true)
    prop.put("mail.smtp.starttls.enable", true)
    prop.put("mail.smtp.host", host)
    prop.put("mail.smtp.port", port)
    prop.put("mail.smtp.ssl.trust", host)

    // authentication
    val auth = new Authenticator {
      override protected def getPasswordAuthentication: PasswordAuthentication =
        new PasswordAuthentication(user, pass)
    }
    // session

    val session = Session.getInstance(prop, auth)
    // email itself
    val subject = "Email from Alle Vite"
    val content =
      s"""
        <div style="
        border: 1px solid black;
        padding: 20px;
        font-family: sans-serif;
        ">
        <h1> Alle Vite: Password recovery </h1>
        <p> Your password recovery token is $token </p>
        <p> Click <a href=$frontendUrl/login>here > to get back to the application
        <p> from Alle vite</p>
        """

    // message = MIME message
    val message = new MimeMessage(session)
    message.setFrom("sabuj.kumar.jena@gmail.com")
    message.setRecipients(Message.RecipientType.TO, "sabuj.kumar.jena@gmail.com")
    message.setSubject(subject)
    message.setContent(content, "text/html; charset=utf-8")

    // send
    Transport.send(message)
  }

}
