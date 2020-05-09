package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import models.{Booking, Location, Room}
import play.api.db._
import java.text.SimpleDateFormat  
import java.util.Calendar

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
    
    if (qCheckin.compareTo(qCheckout) >=0) BadRequest("Checkin date should be before checkout.")
    if (qCheckin.compareTo(Calendar.getInstance().getTime()) < 0) BadRequest("Checkin date could not be in the past.")

    try{
    // Ahora creamos una variable en donde formulamos nuestra query SQL de búsqueda y la ejecutamos
      val query = conexion.createStatement
      val rooms = query.executeQuery(s"SELECT * FROM Rooms r INNER JOIN Locations l ON r.locationId = l.id WHERE l.code = '$location'")
        
      // Ya con el resultado de la consulta, creamos objetos mascota y los agregamos a la lista de apoyo
      val roomsRes: List[JsValue] = Iterator.continually(rooms).takeWhile(_.next()).map{ rooms =>
        val roomId = rooms.getInt("r.id")
        /*val bookings = query.executeQuery(s"SELECT * FROM bookings WHERE roomId = $roomId")
        
        val bookingsRes: List[Boolean] = Iterator.continually(bookings).takeWhile(_.next()).map{ bookings =>
          val bCheckin = bookings.getDate("checkin")
          val bCheckout = bookings.getDate("checkout")
          if ((qCheckin.compareTo(bCheckin) =< 0  && qCheckout.compareTo(bCheckout) >= 0) || (qCheckin.compareTo(bCheckin) >= 0 && qCheckin.compareTo(bCheckout) < 0) || (qCheckout.compareTo(bCheckin) > 0 && qCheckout.compareTo(bCheckout) =< 0)) true
        }.toList
        
        if (bookingsRes == Nil) {*/
          val json: JsValue = Json.obj(
            "id" -> roomId,
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
              "name" -> "Una Agencia",
              "id" -> 1234
            ),
            "property_name" -> rooms.getString("r.name"),
            "rating" -> rooms.getDouble("r.rating")
          )

          json
        //}
      }.toList
      
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
      
      val resultadoImages = query.executeQuery(s"SELECT url FROM room_images ri WHERE ri.roomId = $id;")      
      while(resultadoImages.next){
        val jsonImage: JsValue = Json.obj(
          "url" -> resultadoImages.getString("url")
        )
        images = images :+ jsonImage
      }      

      val resultadoServices = query.executeQuery(s"SELECT DISTINCT name FROM services_per_room spr INNER JOIN services s ON spr.serviceId = s.id WHERE spr.roomId = $id;")     
      while(resultadoServices.next){
        val jsonService: String = resultadoServices.getString("name")
        services = services :+ jsonService
      } 

      val resultadoRoom = query.executeQuery(s"SELECT DISTINCT * FROM rooms r INNER JOIN locations l ON r.locationId = l.id WHERE r.id = $id;")
      while(resultadoRoom.next){
        val jsonRoom: JsValue = Json.obj(
          "id" -> resultadoRoom.getInt("r.id"),
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
            "name" -> "Una Agencia",
            "id" -> 1234,
            "logo_url" -> "https://banner2.kisspng.com/20180606/yer/kisspng-play-framework-scala-software-framework-java-web-a-play-again-5b1896eec63338.1231269915283381588118.jpg"
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
      case success => Ok("json")
      case e:JsError => BadRequest("No se pudo actualizar porque hay malos parametros!!")
    }.recoverTotal{
      e => BadRequest("Detected error:"+ e + request.body)
    }
  }
}
