package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import models.{Booking, Location, Room}
import play.api.db._
import java.text.SimpleDateFormat  
import java.util.{Calendar, Date}
import java.sql.Timestamp

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class RoomController @Inject()(db: Database,cc: ControllerComponents) extends AbstractController(cc) { 
  def getRooms = Action {
    Ok("ok")
  }

  //Service to search rooms available
  def search(location: String, checkin: String, checkout: String) = Action { implicit request: Request[AnyContent] =>
    // En primer lugar creamos una variable para realizar la conexion con la BD
    val conexion = db.getConnection()
    
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

    val qCheckin = dateFormat.parse(checkin)
    val qCheckout = dateFormat.parse(checkout)
    
    if (qCheckin.compareTo(qCheckout) >=0) {
      BadRequest("Checkin date should be before checkout.")
    } else {
      if (qCheckin.compareTo(Calendar.getInstance().getTime()) < 0) {
        BadRequest("Checkin date could not be in the past.")
      } else {
        try{
        // Ahora creamos una variable en donde formulamos nuestra query SQL de búsqueda y la ejecutamos
          val query = conexion.createStatement
          val query1 = conexion.createStatement
          val rooms = query.executeQuery(s"SELECT * FROM Rooms r INNER JOIN Locations l ON r.locationId = l.id WHERE l.code = '$location'")
            
          // Ya con el resultado de la consulta, creamos objetos mascota y los agregamos a la lista de apoyo
          val roomsRes: List[JsValue] = Iterator.continually(rooms).takeWhile(_.next()).map{ rooms =>
            val roomId = rooms.getInt("r.id")
            val bookings = query1.executeQuery(s"SELECT * FROM Bookings WHERE roomId = $roomId")
            
            val bookingsRes: List[java.sql.ResultSet] = Iterator.continually(bookings).takeWhile(_.next()).filter{ bookings =>
              val bCheckin = bookings.getTimestamp("checkin")
              val bCheckout = bookings.getTimestamp("checkout")

              (qCheckin.compareTo(bCheckin) <= 0  && qCheckout.compareTo(bCheckout) >= 0) || (qCheckin.compareTo(bCheckin) >= 0 && qCheckin.compareTo(bCheckout) < 0) || (qCheckout.compareTo(bCheckin) > 0 && qCheckout.compareTo(bCheckout) <= 0)
            }.toList
            
            if (bookingsRes == Nil) {
              val json: JsValue = Json.obj(
                "id" -> rooms.getString(roomId),
                "thumbnail" -> rooms.getString("r.thumbnail"),
                "location" -> Json.obj(
                  "name" -> rooms.getString("l.name"),
                  "code" -> rooms.getString("l.code"),
                  "latitude" -> rooms.getDouble("l.latitude"),
                  "longitude" -> rooms.getDouble("l.longitude")
                ),
                "price" -> rooms.getDouble("r.price"),
                "currency" -> "COP",
                "agency" -> Json.obj(
                  "name" -> "Agencia Scala",
                  "id" -> 42,
                  "logo_url" -> "https://rentrooms.s3.amazonaws.com/Scala.png"
                ),
                "property_name" -> rooms.getString("r.name"),
                "rating" -> rooms.getDouble("r.rating")
              )

              Some(json)
            }
            else
              None
          }.toList.flatten
          
          val jsonAux = Json.toJson(roomsRes) // Finalmente, se Jsifican los resultados
          Ok(jsonAux) // Y se retorna la lista de habitaciones Jsificada
        }/*
        catch{
          case e: Exception => BadRequest(e.toString())
        }*/
        finally{
          // Antes de retornar los resultados, cerramos la conexión a la BD
          conexion.close()
        }
      }
    }
  }
  
  //Service to search rooms available
  def detail(id: Long) = Action { implicit request: Request[AnyContent] =>
    // En primer lugar creamos una variable para realizar la conexion con la BD
    val conexion = db.getConnection()  
    // A continuación inicializamos (vaciamos) la lista con la que procesaremos los datos que lleguen de la BD
    var images = List[JsValue]()    
    var services = List[String]() 
    var room: JsValue = Json.obj()   
    try{
      val query = conexion.createStatement      
      
      val resultadoImages = query.executeQuery(s"SELECT url FROM Room_Images ri WHERE ri.roomId = $id;")      
      while(resultadoImages.next){
        val jsonImage: JsValue = Json.obj(
          "url" -> resultadoImages.getString("url")
        )
        images = images :+ jsonImage
      }      

      val resultadoServices = query.executeQuery(s"SELECT DISTINCT name FROM Services_Per_Room spr INNER JOIN Services s ON spr.serviceId = s.id WHERE spr.roomId = $id;")     
      while(resultadoServices.next){
        val jsonService: String = resultadoServices.getString("name")
        services = services :+ jsonService
      } 

      val resultadoRoom = query.executeQuery(s"SELECT DISTINCT * FROM Rooms r INNER JOIN Locations l ON r.locationId = l.id WHERE r.id = $id;")
      while(resultadoRoom.next){
        val jsonRoom: JsValue = Json.obj(
          "id" -> resultadoRoom.getString("r.id"),
          "images" -> images,
          "location" -> Json.obj(
            "name" -> resultadoRoom.getString("l.name"),
            "code" -> resultadoRoom.getString("l.code"),
            "latitude" -> resultadoRoom.getDouble("l.latitude"),
            "longitude" -> resultadoRoom.getDouble("l.longitude")
          ),
          "price" -> resultadoRoom.getDouble("r.price"),
          "currency" -> "COP",
          "agency" -> Json.obj(
            "name" -> "Agencia Scala",
            "id" -> 42,
            "logo_url" -> "https://rentrooms.s3.amazonaws.com/Scala.png"
          ),
          "property_name" -> resultadoRoom.getString("r.name"),
          "rating" -> resultadoRoom.getDouble("r.rating"),
          "services" -> services
        )
        room = jsonRoom   
      }
    }
    finally{
      // Antes de retornar los resultados, cerramos la conexión a la BD
      conexion.close()
    }
    val jsonAux = Json.toJson(room) // Finalmente, se Jsifican los resultados
    Ok(jsonAux) // Y se retorna la lista de habitaciones Jsificada    
  }
  
  //Service to search rooms available
  def booking = Action(parse.json) { implicit request =>
    request.body.validate[Booking].map{       
      case success => 
        var nuevaReserva = request.body
        val conexion = db.getConnection() 
        var reserva: JsValue = Json.obj()

        val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

        val qCheckin = dateFormat.parse(nuevaReserva("checkin").as[String])
        val qCheckout = dateFormat.parse(nuevaReserva("checkout").as[String])           
        if(qCheckin.compareTo(qCheckout) >= 0) {
          BadRequest("Checkin date should be before checkout.")
        }else{
          if(qCheckin.compareTo(Calendar.getInstance().getTime()) < 0) {
            BadRequest("Checkin date could not be in the past.")
          }else{
            try{            
              val query = conexion.createStatement
              val resultadosReservasQuery=query.executeQuery(s"SELECT * FROM Bookings bo WHERE roomId = ${nuevaReserva("id_room")};")
              var reservaOcupada = false
              while(resultadosReservasQuery.next){
                var checkinReservado = dateFormat.parse(resultadosReservasQuery.getString("bo.checkin"))
                var checkoutReservado = dateFormat.parse(resultadosReservasQuery.getString("bo.checkout"))
                if(((qCheckin.compareTo(checkinReservado) >= 0) && (qCheckin.compareTo(checkoutReservado) <= 0)) 
                  || ((qCheckout.compareTo(checkinReservado) >= 0) && (qCheckout.compareTo(checkoutReservado) <= 0)) 
                  || ((qCheckin.compareTo(checkinReservado) <= 0) && (qCheckout.compareTo(checkoutReservado) >= 0))){
                  reservaOcupada = true
                }                 
              }
              if(reservaOcupada == true){
                BadRequest("Occupied room.")
              }else{
                val resultadoInsert = query.executeUpdate(s"INSERT INTO Bookings (`name`, `email`, `checkin`, `checkout`, `roomId`) " +
                s"VALUES ( '${nuevaReserva("name").as[String]}', '${nuevaReserva("email").as[String]}', '${nuevaReserva("checkin").as[String]}', '${nuevaReserva("checkout").as[String]}', ${nuevaReserva("id_room")})") 
                val resultadoBusqueda = query.executeQuery("SELECT * FROM Bookings b WHERE (SELECT LAST_INSERT_ID())=b.id")
                while(resultadoBusqueda.next){
                  val checkin = resultadoBusqueda.getString("b.checkin").substring(0, 10)
                  val checkout = resultadoBusqueda.getString("b.checkout").substring(0, 10)
                  val json: JsValue = Json.obj(
                    "id_booking" -> resultadoBusqueda.getString("b.id"),
                    "checkin" -> checkin,
                    "checkout" -> checkout,
                    "email" -> resultadoBusqueda.getString("b.email"),
                    "name" -> resultadoBusqueda.getString("b.name"),
                    "id_room" -> resultadoBusqueda.getInt("b.roomId"),
                  )
                  reserva = json
                }
                val jsonAux = Json.toJson(reserva) // Finalmente, se Jsifican los resultados
                Ok(jsonAux) // Y se retorna la lista de habitaciones Jsificada  
              }
            }
            finally{
              // Antes de retornar los resultados, cerramos la conexión a la BD
              conexion.close()
            }
            
          }
        }
        

        
      case e:JsError => BadRequest("Error")
    }.recoverTotal{
      e:JsError => BadRequest("Error")
    }
  }
}
