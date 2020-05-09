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
  ('Barranquilla', 'BAQ', 10.963889, -74.796387);
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
    'Casita Cara',
    1,
    'Una Casita muy cara y muy maluca',
    4.5,
    600000,
    'https://d3fky3asuafjls.cloudfront.net/fHbOcDWDNHGJiAU2-L-QuOzoe-Y=/200x130/02/97/41/02974186.jpg'
  ),
  (
    'Casita Buena',
    2,
    'Una Casita buena y barata',
    10,
    250000,
    'https://d3fky3asuafjls.cloudfront.net/fHbOcDWDNHGJiAU2-L-QuOzoe-Y=/200x130/02/97/41/02974186.jpg'
  ),
  (
    'Casita Pobre',
    3,
    'Una Casita muy fea y pero humilde',
    7.6,
    75000,
    'https://d3fky3asuafjls.cloudfront.net/fHbOcDWDNHGJiAU2-L-QuOzoe-Y=/200x130/02/97/41/02974186.jpg'
  );
INSERT INTO `Room_Images`(`roomId`, `url`)
VALUES
  (
    1,
    'https://d3fky3asuafjls.cloudfront.net/4bhKa1iNdn5G-Ge864anLh6tkcI=/720x480/03/17/17/03171799.jpg'
  ),
  (
    1,
    'https://d3fky3asuafjls.cloudfront.net/Nfq4Z0vvbuvYgPQhoB4vlPLTiAI=/720x480/03/17/18/03171803.jpg'
  ),
  (
    1,
    'https://d3fky3asuafjls.cloudfront.net/3RgFAdn21O8sL_hNY9lVrSbglaA=/720x480/03/17/18/03171804.jpg'
  ),
  (
    2,
    'https://d3fky3asuafjls.cloudfront.net/4bhKa1iNdn5G-Ge864anLh6tkcI=/720x480/03/17/17/03171799.jpg'
  ),
  (
    2,
    'https://d3fky3asuafjls.cloudfront.net/Nfq4Z0vvbuvYgPQhoB4vlPLTiAI=/720x480/03/17/18/03171803.jpg'
  ),
  (
    3,
    'https://d3fky3asuafjls.cloudfront.net/4bhKa1iNdn5G-Ge864anLh6tkcI=/720x480/03/17/17/03171799.jpg'
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
  (1, 2),
  (1, 3),
  (2, 1),
  (3, 1),
  (3, 2),
  (4, 2),
  (4, 3),
  (5, 3),
  (6, 1),
  (6, 2),
  (6, 3);

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
