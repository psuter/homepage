package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import play.api.libs.concurrent.Akka

import play.api.Play.current

import scala.concurrent.ExecutionContext

object Mots extends Controller {
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  def index = Action {
    Ok(views.html.mots())
  }

  def sixletters = Action {
    Ok(views.html.sixletters())
  }

  def json = Action(parse.json) { request =>
    val body = request.body

    val minLength  = (body \ "minLength").asOpt[Int].filterNot(_ <= 0)
    val maxLength  = (body \ "maxLength").asOpt[Int].filterNot(_ <= 0)
    val contains   = (body \ "contains").asOpt[String]
    val distinct   = (body \ "distinct").asOpt[Boolean].getOrElse(false)
    val palindrome = (body \ "palindrome").asOpt[Boolean].getOrElse(false)
    val ordering   = (body \ "ordering").asOpt[String].filterNot(_ == "n")

    val words = Akka.future {
      models.mots.Mots.query(minLength, maxLength, contains, distinct, palindrome, ordering)
    }

    Async {
      words.map { ws =>
        val response = JsObject(Seq(
          "count" -> JsNumber(ws.size),
          "words" -> JsArray(ws.map(JsString(_)))
        ))

        Ok(response).as("application/json")
      }
    }
  }
}
