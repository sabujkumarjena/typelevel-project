package com.allevite.jobsboard.core

import cats.*
import cats.implicits.*
import com.allevite.jobsboard.config.StripeConfig
import org.typelevel.log4cats.Logger
import com.stripe.Stripe as TheStripe
import com.stripe.model.checkout.Session
import com.stripe.param.checkout.SessionCreateParams
import com.allevite.jobsboard.logging.syntax.*

trait Stripe[F[_]] {
  /*
  1.  Someone calls an endpoint on our server
      (send a JobInfo to us) - persisted to the DB - Jobs[F].create(...)
  2.  Return a checkout page URL
  3.  The frontend will redirect the user to that URL
  4.  the user pays(fill in cc details..)
  5.  the backend will be notified by Stripe (webhook)
      - test mode: use Stripe CLI to redirect the events to localhost:4041/1pi/jobs/webhook..
  6.  Perform the final operation on the job advert - set the active flag to false for that job id
      app -> http -> stripe -> redirect user
                                            <- user pays stripe
      activate job  <- webhook <- stripe
   */
  def createCheckoutSession(jobId: String, userEmail: String): F[Option[Session]]
}
/*
SessionCreateParams params =
          SessionCreateParams.builder()
            .setMode(SessionCreateParams.Mode.PAYMENT)
            .setSuccessUrl(YOUR_DOMAIN + "/success.html")
            .setCancelUrl(YOUR_DOMAIN + "/cancel.html")
            .addLineItem(
              SessionCreateParams.LineItem.builder()
                .setQuantity(1L)
                // Provide the exact Price ID (for example, pr_1234) of the product you want to sell
                .setPrice("{{PRICE_ID}}")
                .build())
            .build();
      Session session = Session.create(params);
 */
// globally set constant

class LiveStripe[F[_]: MonadThrow: Logger] private (
    key: String,
    price: String,
    successUrl: String,
    cancelUrl: String
) extends Stripe[F] {
  TheStripe.apiKey = key
  override def createCheckoutSession(jobId: String, userEmail: String): F[Option[Session]] = {
    SessionCreateParams
      .builder()
      .setMode(SessionCreateParams.Mode.PAYMENT)
      // automatic receipt/invoice
      .setInvoiceCreation(
        SessionCreateParams.InvoiceCreation.builder().setEnabled(true).build()
      )
      // save the user email
      .setPaymentIntentData(
        SessionCreateParams.PaymentIntentData.builder().setReceiptEmail(userEmail).build()
      )
      .setSuccessUrl(s"$successUrl/$jobId") // need config
      .setCancelUrl(cancelUrl)   // need config
      .setCustomerEmail(userEmail)
      .setClientReferenceId(jobId) // will be sent back to me by the webhook
      .addLineItem(
        SessionCreateParams.LineItem
          .builder()
          .setQuantity(1L)
          // Provide the exact Price ID (for example, pr_1234) of the product you want to sell
          .setPrice(price) // need config
          .build()
      )
      .build()
      .pure[F]
      .map(params => Session.create(params))
      .map(_.some)
      .logError(error => s"Creating Checkout session failed: $error")
      .recover { case _ => None }

  }
}

object LiveStripe {
  def apply[F[_]: MonadThrow: Logger](stripeConfig: StripeConfig): F[LiveStripe[F]] =
    new LiveStripe[F](
      stripeConfig.key,
      stripeConfig.price,
      stripeConfig.successUrl,
      stripeConfig.cancelUrl
    ).pure[F]
}
