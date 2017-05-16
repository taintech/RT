package controllers

import javax.inject._

import play.api._
import play.api.libs.json.{Json, Writes}
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.Logger
import play.api.cache.CacheApi

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class FlightsController @Inject()(ws: WSClient, cache: CacheApi) extends Controller {

  val ROUTES_URL = "https://api.ryanair.com/core/3/routes/"
  val ROUTES_CACHE = "ryanair.routes"

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

  case class FlightTrip(stops: Int, legs: Seq[Connection])

  implicit val flightWrites = new Writes[Connection] {
    def writes(flight: Connection) = Json.obj(
      "departureAirport" -> flight.airportFrom,
      "arrivalAirport" -> flight.airportTo
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
    getConnections.map(e => Ok(Json.toJson(calculateFlightTrips(e, departure, arrival)))).recover {
      case e =>
        Logger.debug("Failed to retrieve connections", e)
        InternalServerError
    }
  }

  private def calculateFlightTrips(connections: Seq[Connection], departure: String, arrival: String): Seq[FlightTrip] = {
    val directFligths = connections.filter(c => c.airportFrom == departure && c.airportTo == arrival)
    val oneConnectionFlights = for {
      c1 <- connections
      if c1.airportTo == arrival
      c2 <- connections
      if c2.airportFrom == departure && c2.airportTo == c1.airportFrom
    } yield Seq(c1, c2)
    Seq(FlightTrip(0, directFligths)) ++ oneConnectionFlights.map(e => FlightTrip(1, e))
  }

  private def getConnections: Future[Seq[Connection]] = cache.get[Seq[Connection]](ROUTES_CACHE) match {
    case Some(conns) => Future(conns)
    case None => {
      val routesRequest = ws.url(ROUTES_URL)
      routesRequest.get().map {
        case response if response.status == OK => {
          val connections = response.json.as[Seq[Connection]]
          cache.set(ROUTES_CACHE, connections, 1 day)
          connections
        }
        case response => throw new Exception(s"RyanAir routes service failed with error code: ${response.status}")
      }
    }
  }

}
