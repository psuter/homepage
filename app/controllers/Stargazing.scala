package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import play.api.libs.concurrent.Akka

import play.api.Play.current

import scala.concurrent.ExecutionContext

object Stargazing extends Controller {
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  def index = Action { implicit request =>
    Ok(views.html.fics())
  }

  def socket = WebSocket.async[JsValue] { implicit request =>
    models.stargazing.Stargazing.watch()
  }
}
