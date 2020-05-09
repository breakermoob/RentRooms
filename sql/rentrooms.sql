--
-- Drop Base de datos: `rentrooms`
--
-- ALTER TABLE `Room_Images` DROP FOREIGN KEY `Room_Images_fk0`;
-- ALTER TABLE `Services_Per_Room` DROP FOREIGN KEY `Room_Services_fk0`;
-- ALTER TABLE `Locations` DROP FOREIGN KEY `Locations_fk0`;
DROP TABLE IF EXISTS `Locations`;
DROP TABLE IF EXISTS `Services_Per_Room`;
DROP TABLE IF EXISTS `Room_Images`;
DROP TABLE IF EXISTS `Services`;
DROP TABLE IF EXISTS `Rooms`;
--
-- Base de datos: `rentrooms`
--
-- --------------------------------------------------------
--
-- Estructura de tabla para la tabla `Rooms`
--
CREATE TABLE IF NOT EXISTS Rooms (
  `id` INT(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `locationId` INT(50) NOT NULL,
  `description` varchar(255) NOT NULL,
  `rating` DOUBLE,
  `price` DOUBLE NOT NULL,
  `thumbnail` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = latin1;
-- --------------------------------------------------------
--
-- Estructura de tabla para la tabla `RoomImages`
--
CREATE TABLE IF NOT EXISTS `Room_Images` (
  `id` INT(50) NOT NULL AUTO_INCREMENT,
  `roomId` INT(50) NOT NULL,
  `url` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = latin1;
-- --------------------------------------------------------
--
-- Estructura de tabla para la tabla `RoomServices`
--
CREATE TABLE IF NOT EXISTS `Services` (
  `id` INT(50) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = latin1;
CREATE TABLE IF NOT EXISTS `Services_Per_Room` (
  `id` INT(50) NOT NULL AUTO_INCREMENT,
  `serviceId` INT(50) NOT NULL,
  `roomId` INT(50) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = latin1;
-- --------------------------------------------------------
--
-- Estructura de tabla para la tabla `RoomLocation`
--
CREATE TABLE IF NOT EXISTS `Locations` (
  `id` INT(50) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `code` varchar(25) NOT NULL,
  `latitude` DOUBLE NOT NULL,
  `longitude` DOUBLE NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = latin1;
--
-- Estructura de tabla para la tabla `Bookings`
--
CREATE TABLE IF NOT EXISTS `Bookings` (
  `id` INT(50) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `email` varchar(100) NOT NULL,
  `checkin` TIMESTAMP NOT NULL,
  `checkout` TIMESTAMP NOT NULL,
  `roomId` INT(50) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = latin1;
--
-- Filtros para las tablas
--
ALTER TABLE `Bookings`
ADD
  CONSTRAINT `Bookings_fk0` FOREIGN KEY (`roomId`) REFERENCES `Rooms`(`id`) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE `Room_Images`
ADD
  CONSTRAINT `Room_Images_fk0` FOREIGN KEY (`roomId`) REFERENCES `Rooms`(`id`) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE `Services_Per_Room`
ADD
  CONSTRAINT `Services_Per_Room_fk0` FOREIGN KEY (`serviceId`) REFERENCES `Services`(`id`) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE `Services_Per_Room`
ADD
  CONSTRAINT `Services_Per_Room_fk1` FOREIGN KEY (`roomId`) REFERENCES `Rooms`(`id`) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE `Rooms`
ADD
  CONSTRAINT `Rooms_fk0` FOREIGN KEY (`locationId`) REFERENCES `Locations`(`id`) ON DELETE CASCADE ON UPDATE CASCADE;
