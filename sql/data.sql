-- Data
  --
INSERT INTO `Locations`(
    `name`,
    `code`,
    `latitude`,
    `longitude`
  )
VALUES
  ('Medellin', 'MDE', 6.230833, -75.590553),
  ('Bogota', 'BOG', 4.624335, -74.063644),
  ('Cali', 'CLO', 3.359889, -76.638565),
  ('Barranquilla', 'BAQ', 10.963889, -74.796387),
  ('Cartagena', 'CTG', 10.3997200, -75.5144400),
  ('Cucuta', 'CUC', 7.8939100, -72.5078200),
  ('Soledad', 'SLD', 10.0142900, -73.5522800),
  ('Ibague', 'IBE', 4.435,-75.217);
INSERT INTO `Rooms`(
    `name`,
    `locationId`,
    `description`,
    `rating`,
    `price`,
    `thumbnail`
  )
VALUES
  (
    'Vendo Casa Grande Santa Monica',
    1,
    'Casa para 4 personas, muy comoda con multiples servicios incluidos',
    4.5,
    600000,
    'https://rentrooms.s3.amazonaws.com/MDE/MDE-1-tub.jpg'
  ),
  (
    'Hostal Bogota Real',
    2,
    'Hostal comodo y tranquilo con todos los servicios incluidos',
    4.1,
    350000,
    'https://rentrooms.s3.amazonaws.com/BOG/BOG-1-tub.jpg'
  ),
  (
    'Hilton Garden Inn Cali',
    3,
    'Hotel de lujo con todos los servicios, sala de juntas, espacios de trabajo perzonalizados y con transporte las 24 horas',
    5,
    1200000,
    'https://rentrooms.s3.amazonaws.com/CLO/CLO-1-tub.jpg'
  ),
  (
    'Casa Bohemia Hotel',
    5,
    'Hotel comodo, un lugar ideal para descansar con tu familia, incluye todos los servicios; wifi, parqueadero, zona humeda, bar, etc',
    3.5,
    800000,
    'https://rentrooms.s3.amazonaws.com/CTG/CTG-1-tub.jpg'
  );
INSERT INTO `Room_Images`(`roomId`, `url`)
VALUES
  (
    1,
    'https://rentrooms.s3.amazonaws.com/MDE/MDE-1-1.jpg'
  ),
  (
    1,
    'https://rentrooms.s3.amazonaws.com/MDE/MDE-1-2.jpg'
  ),
  (
    1,
    'https://rentrooms.s3.amazonaws.com/MDE/MDE-1-3.jpg'
  ),
  (
    2,
    'https://rentrooms.s3.amazonaws.com/BOG/BOG-1-1.jpg'
  ),
  (
    2,
    'https://rentrooms.s3.amazonaws.com/BOG/BOG-1-2.jpg'
  ),
  (
    3,
    'https://rentrooms.s3.amazonaws.com/CLO/CLO-1-1.jpg'
  ),
  (
    3,
    'https://rentrooms.s3.amazonaws.com/CLO/CLO-1-2.jpg'
  ),
  (
    3,
    'https://rentrooms.s3.amazonaws.com/CLO/CLO-1-3.jpg'
  ),
  (
    3,
    'https://rentrooms.s3.amazonaws.com/CLO/CLO-1-4.jpg'
  ),
  (
    4,
    'https://rentrooms.s3.amazonaws.com/CTG/CTG-1-1.jpg'
  ),
  (
    4,
    'https://rentrooms.s3.amazonaws.com/CTG/CTG-1-2.jpg'
  ),
  (
    4,
    'https://rentrooms.s3.amazonaws.com/CTG/CTG-1-3.jpg'
  );
INSERT INTO `Services`(`id`, `name`)
VALUES
  (1, 'Wifi'),
  (2, 'Zona Humeda'),
  (3, 'Bar'),
  (4, 'Desayuno Gratis'),
  (5, 'Parqueadero'),
  (6, 'Zona para fumadores');
INSERT INTO `Services_Per_Room`(`serviceId`, `roomId`)
VALUES
  (1, 1),
  (2, 1),
  (3, 1),
  (6, 1),
  (1, 2),
  (5, 2),
  (6, 2),
  (1, 3),
  (2, 3),
  (3, 3),
  (4, 3),
  (5, 3),
  (6, 3),
  (1, 4),
  (2, 4),
  (3, 4),
  (4, 4),
  (5, 4),
  (6, 4);

  INSERT INTO `Bookings`(
    `name`,
    `email`,
    `checkin`,
    `checkout`,
    `roomId`
  )
VALUES
  ('Juan Roman', 'jd@scala.com', '2008-01-01 00:00:01', '2008-01-01 00:00:01',1),
  ('Diego Maradona', 'dm@scala.com', '2008-01-01 00:00:01', '2008-01-01 00:00:01',2);
