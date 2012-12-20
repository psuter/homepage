package controllers

import play.api._
import play.api.mvc._

object Homepage extends Controller {
  
  def index = Action {
    Redirect("http://lara.epfl.ch/~psuter/")
  }
  
}
