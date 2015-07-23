package fr.acinq.aws.ses

import com.amazonaws.services.simpleemail.AmazonSimpleEmailService
import com.amazonaws.services.simpleemail.model._

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by PM on 21/07/2015.
 */
case class Email(from: String, to: String, subject: String, text: String, html: String) {

  def send()(implicit executionContext: ExecutionContext = ExecutionContext.Implicits.global, ses: AmazonSimpleEmailService): Future[SendEmailResult] = Future {
    val subjContent = new Content().withData(subject)
    val msg = new Message().withSubject(subjContent)
    val textContent = new Content().withData(text)
    val htmlContent = new Content().withData(html)
    val body = new Body().withHtml(htmlContent).withText(textContent)
    msg.setBody(body)
    val req = new SendEmailRequest(from, new Destination().withToAddresses(to), msg)
    ses.sendEmail(req)
  }
}

