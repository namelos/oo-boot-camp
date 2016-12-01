import java.util.UUID

class ParkingLot(slots: Int = 4) {
  var cars: List[(UUID, Car)] = List()

  def park(car: Car) =
    if (emptySlots > 0) {
      val token = UUID.randomUUID()
      cars = (token, car) :: cars
      token
    } else throw new Exception("There is no room.")

  def withdraw(token: UUID) = cars.find(_._1 == token) match {
    case Some((_, car)) => car
    case None           => throw new Exception("No such car.")
  }

  def emptySlots = slots - cars.length
}

class Car
