package controllers

import play.api._
import play.api.mvc._

object Homepage extends Controller {
  def index = Action { implicit request =>
    // Not quite ready for prime time...
    // Ok(views.html.home())
    Redirect("http://lara.epfl.ch/~psuter/")
  }
  
}
