--Consultas
--Get all Rooms with location
SELECT
  *
FROM Rooms
JOIN Locations ON LocationId = Locations.id;
--
  --Get all Rooms searching by string name or description
SELECT
  *
FROM Rooms
JOIN Locations ON LocationId = Locations.id
WHERE
  Rooms.name LIKE '%po%'
  OR Rooms.description LIKE '%po%';
--
  --Get all Rooms searching by string name or description with locations filter
SELECT
  *
FROM Rooms
JOIN Locations ON LocationId = Locations.id
WHERE
  Rooms.name LIKE '%po%'
  OR Rooms.description LIKE '%malu%'
  AND Locations.name LIKE '%med%';
--
  --Get all Rooms with city filter
SELECT
  *
FROM Rooms
JOIN Locations ON LocationId = Locations.id
WHERE
  Locations.name LIKE '%al%';
--
  --Get all Rooms with city and code filter
SELECT
  *
FROM Rooms
JOIN Locations ON LocationId = Locations.id
WHERE
  Locations.code = 'MDE'
  or Locations.name LIKE '%al%';
--
  --GET aLL Rooms with Location and Images
  --Esta no tiene mucho sentido porque lo mejor seria buscar los servicio e imagenes por aparte
SELECT
  *
FROM Rooms AS R
INNER JOIN Locations AS L ON R.LocationId = L.id
INNER JOIN Room_Images AS I ON I.roomId = R.id
WHERE
  Locations.code = 'MDE'
  or Locations.name LIKE '%al%';
--GET aLL Rooms with Location and Images with filters
  --Esta no tiene mucho sentido porque lo mejor seria buscar los servicio e imagenes por aparte
SELECT
  *
FROM Rooms AS R
INNER JOIN Locations AS L ON R.LocationId = L.id
INNER JOIN Room_Images AS I ON I.roomId = R.id;
WHERE
  (
    R.name LIKE '%po%'
    OR R.description LIKE '%po%'
  )
  AND L.name LIKE '%med%';
--Get all images of one Roome
SELECT
  url
FROM Room_Images
INNER JOIN Rooms AS R ON roomId = R.id
WHERE
  R.id = 1;
--Get all services of one Room
SELECT
  S.name AS RoomsServices
FROM Services AS S
INNER JOIN Services_Per_Room AS P ON S.id = P.serviceId
INNER JOIN Rooms AS R ON R.id = P.roomId
WHERE
  R.id = 1;
--Get all of one Room
65