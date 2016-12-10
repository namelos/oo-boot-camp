import java.util.UUID
import scalaz._
import Scalaz._

class ParkingLot(slots: Int = 1) {
  var cars: List[(UUID, Car)] = List()

  def park(car: Car) = cars match {
    case cs if notFull =>
      val id = UUID.randomUUID()
      cars = (id, car) :: cs
      id some
    case _           => none
  }

  def pick(id: UUID) = {
    cars find (_._1 == id) match {
      case Some((id, car)) =>
        cars = cars filter {case (i, _) => i != id}
        car some
      case _ => none
    }
  }

  def notFull = cars.length != slots

  def availableSlots = slots - cars.length
}

class BaseParkingBoy(parkingLots: ParkingLot*) {

}

class ParkingBoy(parkingLots: ParkingLot*) extends BaseParkingBoy(parkingLots: _*) {
  def park(car: Car) = parkingLots find(_ notFull) flatMap(_ park car)
}

class SmartParkingBoy(parkingLots: ParkingLot*) {
  def park(car: Car) = (parkingLots sortBy (_ availableSlots) last) park car
}

class Car
