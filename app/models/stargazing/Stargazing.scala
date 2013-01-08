package models.stargazing

import akka.actor._
import scala.concurrent.duration._

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._

import akka.util.Timeout
import akka.pattern.ask

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

// Messages
case class Join()
case class Quit(id : Int)

case class Connected(id : Int, enumerator : Enumerator[JsValue])
case class CannotConnect(msg : String)

object Stargazing {
  implicit val timeout = Timeout(1 second)
  
  lazy val default = Akka.system.actorOf(Props[Stargazing])

  def watch() : scala.concurrent.Future[(Iteratee[JsValue,_],Enumerator[JsValue])] = {

    (default ? Join()).map {
      
      case Connected(id, enumerator) => 
      
        val iteratee = Iteratee.foreach[JsValue](event => ()).mapDone { _ =>
          default ! Quit(id)
        }
        (iteratee, enumerator)
        
      case CannotConnect(error) => 
        val iteratee   = Done[JsValue,Unit]((), Input.EOF)
        val enumerator = Enumerator[JsValue](JsObject(Seq("error" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))
        (iteratee, enumerator)
    }
  }
}

class Stargazing extends Actor {
  private var stargazers = Map.empty[Int,PushEnumerator[JsValue]]
  private def nextID : Int = if(stargazers.isEmpty) 0 else (stargazers.keySet.max + 1)
  
  def receive = {
    case Join() =>
      val id = nextID
      val channel = Enumerator.imperative[JsValue]()
      stargazers = stargazers + (id -> channel)
      println(id + " is now connected")
      sender ! Connected(id, channel)
    
    case Quit(id) =>
      println(id + " has now left")
      stargazers = stargazers - id
  }
  
  def notifyAll(text : String) {
    val msg = JsObject(
      Seq(
        "msg" -> JsString(text)
      )
    )
    stargazers.foreach { 
      case (_, channel) => channel.push(msg)
    }
  }
}
