package controllers

import javax.inject._
import play.api._
import play.api.mvc._

@Singleton
class ApplicationController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def sayHello = Action { implicit request: Request[AnyContent] =>
    Ok("Hello there")
  }

}