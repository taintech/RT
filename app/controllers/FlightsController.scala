package controllers

import javax.inject._

import play.api._
import play.api.libs.json.{JsArray, Json, Writes}
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.Logger

class FlightsController @Inject()(ws: WSClient) extends Controller {

  val ROUTES_URL = "https://api.ryanair.com/core/3/routes/"

  implicit val context = play.api.libs.concurrent.Execution.Implicits.defaultContext

  implicit val connectionReads = Json.reads[Connection]

  case class Connection(
    airportFrom: String,
    airportTo: String,
    connectingAirport: Option[String],
    newRoute: Boolean,
    seasonalRoute: Boolean
  )

  case class Flight(departureAirport: String, arrivalAirport: String)

  case class FlightTrip(stops: Int, legs: Seq[Flight])
  object FlightTrip {
    def apply(stops: Int, connections: List[Connection]): FlightTrip =
      FlightTrip(stops, connections.map { c =>
        Flight(c.airportFrom, c.airportTo)
      })
  }

  implicit val flightWrites = new Writes[Flight] {
    def writes(flight: Flight) = Json.obj(
      "departureAirport" -> flight.departureAirport,
      "arrivalAirport" -> flight.arrivalAirport
    )
  }

  implicit val flightTripWrites = new Writes[FlightTrip] {
    def writes(trip: FlightTrip) = Json.obj(
      "stops" -> trip.stops,
      "legs" -> trip.legs
    )
  }

  def interconnections(departure: String, arrival: String) = Action.async { implicit request =>
    Logger.debug(s"departure: $departure, arrival: $arrival")
    val routesRequest = ws.url(ROUTES_URL)
    routesRequest.get().map {
      case response if response.status == OK => {
        val connections = response.json.as[Seq[Connection]]
        val directFligths = connections.filter(c => c.airportFrom == departure && c.airportTo == arrival)
        val oneConnectionFlights = for {
          c1 <- connections
          if c1.airportTo == arrival
          c2 <- connections
          if c2.airportFrom == departure && c2.airportTo == c1.airportFrom
        } yield Seq(c1, c2)
        val result = Seq(FlightTrip(0, directFligths.toList)) ++ oneConnectionFlights.map(e => FlightTrip(1, e.toList))
        Ok(Json.toJson(result))
      }
      case _ => InternalServerError
    }
  }

}
