import java.util.UUID
import scalaz._

class ParkingLot(slots: Int = 1) {
  var cars: List[(UUID, Car)] = List()

  def park(car: Car) = cars match {
    case cs if notFull =>
      val id = UUID.randomUUID()
      cars = (id, car) :: cs
      Some(id)
    case _             => None
  }

  def pick(id: UUID) = {
    cars find (_._1 == id) match {
      case Some((id, car)) =>
        cars = cars filter {case (i, _) => i != id}
        Some(car)
      case _               => None
    }
  }

  def notFull = cars.length != slots

  def availableSlots = slots - cars.length

  def emptyRate = availableSlots.toDouble / slots.toDouble
}

trait Pickable {
  val parkingLots: Seq[ParkingLot]

  def pick(id: UUID) = parkingLots flatMap(_ pick id) headOption
}

class ParkingBoy(val parkingLots: ParkingLot*) extends Pickable {
  def park(car: Car) = parkingLots find(_ notFull) flatMap (_ park car)
}

class SmartParkingBoy(val parkingLots: ParkingLot*) extends Pickable {
  def park(car: Car) = (parkingLots sortBy (_ availableSlots) last) park car
}

class SuperParkingBoy(val parkingLots: ParkingLot*) extends Pickable {
  def park(car: Car) = (parkingLots sortBy (_ emptyRate) last) park car
}

class Car
