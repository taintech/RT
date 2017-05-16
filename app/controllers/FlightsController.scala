package controllers

import javax.inject._

import play.api._
import play.api.libs.ws.WSClient
import play.api.mvc._


class FlightsController @Inject()(ws: WSClient) extends Controller{

  def interconnections = Action { implicit request =>
    Ok("test")
  }

}
