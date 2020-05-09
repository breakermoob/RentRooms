package models

// Importes necesarios para hacer el modelo Room
import com.sun.org.apache.xpath.internal.operations.Or
import play.api.libs.json.{JsPath, Json, Reads}
import play.api.libs.functional.syntax._


// Se crea la clase Booking, en la cual los parámetros deben coincidir con los campos de la tabla Bookings de la base de datos
case class Booking(checkin: String, checkout: String, email: String, name: String, id_room:Long)

// También se crea un objeto Booking con el fin de implementar los métodos para escribir Booking como si fueran Jsons
object Booking {
  implicit val bookingWrite = Json.writes[Booking]
  implicit val bookingRead = Json.reads[Booking]
}

// Se crea la clase Room, en la cual los parámetros deben coincidir con los campos de la tabla Rooms de la base de datos
case class Room(id: Int, name: String, locationId: Int, description: String, rating: Double, price: Double, thumbnail: String)

// También se crea un objeto Room con el fin de implementar los métodos para escribir Room como si fueran Jsons
object Room {
  implicit val roomWrite = Json.writes[Room]
  implicit val roomRead = Json.reads[Room]
}

case class Location(id: Int, name: String, code: String, latitude: Double, longitude: Double)

object Location {
  implicit val locationWrite = Json.writes[Location]
  implicit val locationRead = Json.reads[Location]
}
