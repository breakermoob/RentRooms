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
import java.io.{File, FileInputStream, InputStream}

import com.google.firebase.{FirebaseApp, FirebaseOptions}
import com.google.firebase.database._
import com.google.firebase.auth.FirebaseAuth
import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.auth.oauth2.GoogleCredentials
import org.joda.time.{DateTime, Days}
//import com.google.firebase.tasks.Tasks

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class RoomController @Inject()(db: Database,cc: ControllerComponents) extends AbstractController(cc) { 
  def getRooms = Action {
    //println(setFireBaseConnection)
    val a = verifyIdToken("eyJhbGciOiJSUzI1NiIsImtpZCI6Ijc0Mzg3ZGUyMDUxMWNkNDgzYTIwZDIyOGQ5OTI4ZTU0YjNlZTBlMDgiLCJ0eXAiOiJKV1QifQ.eyJuYW1lIjoiREFOSUVMIEZFTElQRSBKQVJBTUlMTE8gw4FMVkFSRVoiLCJwaWN0dXJlIjoiaHR0cHM6Ly9saDMuZ29vZ2xldXNlcmNvbnRlbnQuY29tL2EtL0FPaDE0R2pWOWxpZ1RCbEhLR2dDaTNrTmZvUkxlbnJZVHRLdWkxSGVncmlPTXc9czk2LWMiLCJpc3MiOiJodHRwczovL3NlY3VyZXRva2VuLmdvb2dsZS5jb20vcmVudHJvb21zLTIwMTkyIiwiYXVkIjoicmVudHJvb21zLTIwMTkyIiwiYXV0aF90aW1lIjoxNTkxMzk4Mjc4LCJ1c2VyX2lkIjoiSUVTcE9YZXN3a1VHaGVoYVE2YmVjMGNpYXVVMiIsInN1YiI6IklFU3BPWGVzd2tVR2hlaGFRNmJlYzBjaWF1VTIiLCJpYXQiOjE1OTEzOTgyNzksImV4cCI6MTU5MTQwMTg3OSwiZW1haWwiOiJkYW5pZWwuamFyYW1pbGxvN0B1ZGVhLmVkdS5jbyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJmaXJlYmFzZSI6eyJpZGVudGl0aWVzIjp7Imdvb2dsZS5jb20iOlsiMTA2MTcyNDUxMTkzNzg3MzI2ODgzIl0sImVtYWlsIjpbImRhbmllbC5qYXJhbWlsbG83QHVkZWEuZWR1LmNvIl19LCJzaWduX2luX3Byb3ZpZGVyIjoiZ29vZ2xlLmNvbSJ9fQ.W8hLq6ufDld0LG7oWSevTp2mZeI1T8WmpnvaHbvbvLKYsUOcAhSEuH6AKTMSqPCLGQh80_sCIH6c5iIm6whZbt4hQ-hY3VVK8MJWeDUyPhR-6Z_CxySpTxg2DSETj1o75PAE_EV6rUrdgYK8ZcEmoMiv0EKYpJT7QVt377umbgZ6-dBdzVXYMlvvGzo48iNposGz5iru5dBiac7uKwXvrWsFP-I4Z8YSYPzZ_Vth_LvfHxNSlSvdBchgR5sbE4IGI03_1rHMNxfKspoLAwFnxaJvLTD42AWKxmsC9t_o6iaQ6-XJV2tBAlwpTlxoPEOQalXmu1zz66mISmD9Lc8T5A")

      println(a)
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
    val token = request.headers.get("authtoken")
    token match  {
      case None =>
        BadRequest("No se encontró token")
      case Some(authtoken)  =>
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

  def getBooking(email: String) = Action { implicit request: Request[AnyContent] =>
    val token = request.headers.get("authtoken")
    token match  {
      case None =>
        BadRequest("No se encontró token")
      case Some(authtoken)  =>
        if(verifyIdToken(authtoken) != email) {
          // Respuesta mala
          BadRequest("Token inválido")
        } else {
          val conexion = db.getConnection()
          var boobkingJson = List[JsValue]()
          try {
            val query = conexion.createStatement
            val query1 = conexion.createStatement
            val resultadoBookings = query.executeQuery(s"SELECT * FROM Bookings b WHERE email = '$email';")

            while (resultadoBookings.next) {
              val roomId = resultadoBookings.getInt("b.roomId")
              val resultadoRooms = query1.executeQuery(s"SELECT * FROM Rooms r INNER JOIN Locations l ON r.locationId = l.id WHERE r.id = $roomId")

              while(resultadoRooms.next){
                val checkin = resultadoBookings.getString("b.checkin").substring(0, 10)
                val checkout = resultadoBookings.getString("b.checkout").substring(0, 10)
                val numDays = countDays(checkin, checkout)
                println(numDays)
                val totalPrice :Double = numDays match {
                  case None => 0.0 // CERO si el metodo countDays FALLA, lo cual es muy poco probable ya que las fechas de la BD ya deben tener el formato correcto
                  case Some(nDays) => nDays*resultadoRooms.getDouble("r.price") // El numero de dias x el precio de estancia del inmueble
                }
                val json: JsValue = Json.obj(
                  "id_room" -> resultadoRooms.getString(roomId),
                  "thumbnail" -> resultadoRooms.getString("r.thumbnail"),
                  "location" -> Json.obj(
                    "name" -> resultadoRooms.getString("l.name"),
                    "code" -> resultadoRooms.getString("l.code"),
                    "latitude" -> resultadoRooms.getDouble("l.latitude"),
                    "longitude" -> resultadoRooms.getDouble("l.longitude")
                  ),
                  "price" -> resultadoRooms.getDouble("r.price"),
                  "currency" -> "COP",
                  "agency" -> Json.obj(
                    "name" -> "Agencia Scala",
                    "id" -> 42,
                    "logo_url" -> "https://rentrooms.s3.amazonaws.com/Scala.png"
                  ),
                  "property_name" -> resultadoRooms.getString("r.name"),
                  "id_booking" -> resultadoBookings.getString("b.id"),
                  "checkin" -> checkin,
                  "checkout"-> checkout,
                  "total_price" -> totalPrice
                )
                boobkingJson = boobkingJson :+ json
              }

            }
            val jsonAux = Json.toJson(boobkingJson)
            Ok(jsonAux)
          } catch {
            case e:Exception=>
              BadRequest("Ocurrió un error")
          } finally{
            // Antes de retornar los resultados, cerramos la conexión a la BD
            conexion.close()
          }
        }
    }
  }

  def countDays(date1: String, date2: String): Option[Int] = {
    try
      {
        // En primer lugar tokenizamos la 1er fecha (ya sea que los delimitadores sean: '-' o '/')
        val aux1 = date1.split(Array('-', '/'))
        //val day1 = aux1(0).toInt // El 1er token debe ser el dia
        //val month1 = aux1(1).toInt // El 2do token debe ser el mes
        //val year1 = aux1(2).toInt // El 3er token debe ser el ano
        val year1 = aux1(0).toInt // El 3er token debe ser el ano
        val month1 = aux1(1).toInt // El 2do token debe ser el mes
        val day1 = aux1(2).toInt // El 1er token debe ser el dia


        // Ahora, creamos un objeto datatime con los datos que obtuvimos de la primera fecha
        val jodaDate1 = new DateTime(year1, month1, day1, 0, 0)

        // Igualmente tokenizamos la 2da fecha
        val aux2 = date2.split(Array('-', '/'))
        val year2 = aux2(0).toInt // El 3er token debe ser el ano
        val month2 = aux2(1).toInt // El 2do token debe ser el mes
        val day2 = aux2(2).toInt // El 1er token debe ser el dia



        // Y creamos otro objeto datatime con los datos que obtuvimos de la segunda fecha
        val jodaDate2 = new DateTime(year2, month2, day2, 0, 0)

        // Finalmente, calculamos la diferencia de fechas (en dias) y retornamos el resultado (el cual estara "envuelto" como un option)
        val difference = Days.daysBetween(jodaDate1, jodaDate2);
        Some(difference.getDays)
      }
    catch
      {
        case _: Throwable => None // En caso de error, se retorna None -nada- (Como indicativo de que sucedio un error)
      }
  }


  // FIREBASE
  def setFireBaseConnection: Boolean = {
    // En primer lugar, revisamos que instancias de Firebase hay vigentes
    val fbApps = FirebaseApp.getApps()
    // Si no hay ninguna instancia de conexion vigente entonces
    if(fbApps.isEmpty) {
      // Establecemos los parametros que necesitamos para conectarnos a FireBase
      val initialFile = new File("rentrooms-20192-firebase-adminsdk-ouoe9-8041ddc0f2.json");
      val credentials: InputStream = new FileInputStream(initialFile);
      val options = new FirebaseOptions.Builder()
      .setCredentials(GoogleCredentials.fromStream(credentials))
      .setDatabaseUrl("https://rentrooms-20192.firebaseio.com")
      .build();
      
      // E intentamos inicializar la conexión con Firebase
      FirebaseApp.initializeApp(options);    
    }

    // Finalmente, retornamos verdadero si en verdad se logro la comunicacion con Firebase o falso en caso contrario
    if (!FirebaseApp.getApps().isEmpty) {true} else {false}
  }

  def verifyIdToken(idToken: String) = {
    // En primer lugar, invocamos el metodo de conexion a Firebase
    setFireBaseConnection

    try
      {
        // Luego, tratamos de decodificar el token de modo que trato de recuperar el correo del mismo y lo retorno
        //val decodedToken = Tasks.await(FirebaseAuth.getInstance().verifyIdToken(idToken))

        val decodedToken = FirebaseAuth.getInstance.verifyIdToken(idToken)
        val uid = decodedToken.getUid
        val email = decodedToken.getEmail()
        email
      }
    catch // En caso de error entonces retorno falso
      {
        case e:Exception=>
          None
      }
  }

}
