FROM openjdk:8

RUN \
  curl -L -o sbt-1.3.9.deb http://dl.bintray.com/sbt/debian/sbt-1.3.9.deb && \
  dpkg -i sbt-1.3.9.deb && \
  rm sbt-1.3.9.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion

WORKDIR /RentRooms

ADD . /RentRooms

CMD sbt run

