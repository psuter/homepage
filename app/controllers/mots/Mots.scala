package controllers.mots

import play.api._
import play.api.mvc._

object Mots extends Controller {

  def index = Action {
    Ok(views.html.main("Mots")("Pouet"))
  }

}
