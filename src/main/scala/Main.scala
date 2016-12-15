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

trait Parkable {
  def findLot(car: Car): Option[ParkingLot]

  def park(car: Car) = findLot(car) flatMap (_ park car)
}

class ParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def findLot(car: Car) = parkingLots find(_ notFull)
}

class SmartParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def findLot(car: Car) = Some(parkingLots sortBy (_ availableSlots) last)
}

class SuperParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def findLot(car: Car) = Some(parkingLots sortBy (_ emptyRate) last)
}

class Car
