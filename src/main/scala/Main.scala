import java.util.UUID
import scalaz._

trait Pickable {
  def pick(id: UUID): Option[Car]
}

trait Parkable {
  def park(car: Car): Option[UUID]
  def canPark: Boolean
}

class ParkingLot(slots: Int = 1) extends Pickable with Parkable {
  var cars: List[(UUID, Car)] = List()

  def park(car: Car): Option[UUID] = cars match {
    case cs if canPark =>
      val id = UUID.randomUUID()
      cars = (id, car) :: cs
      Some(id)
    case _             => None
  }

  def pick(id: UUID): Option[Car] = {
    cars find (_._1 == id) match {
      case Some((id, car)) =>
        cars = cars filter {case (i, _) => i != id}
        Some(car)
      case _               => None
    }
  }

  def canPark: Boolean = cars.length != slots
  def availableSlots: Int = slots - cars.length
  def emptyRate: Double = availableSlots.toDouble / slots.toDouble
  def findLot(car: Car): Option[ParkingLot] = Some(this)
}

class ParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def park(car: Car): Option[UUID] = parkingLots find(_ canPark) flatMap(_ park car)
  def pick(id: UUID): Option[Car] = parkingLots flatMap(_ pick id) headOption
  def canPark: Boolean = parkingLots.exists(_ canPark)
}

class SmartParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def park(car: Car): Option[UUID] = (parkingLots sortBy (_ availableSlots) lastOption) flatMap(_ park car)
  def pick(id: UUID): Option[Car] = parkingLots flatMap(_ pick id) headOption
  def canPark: Boolean = parkingLots.exists(_ canPark)
}

class SuperParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def park(car: Car): Option[UUID] = (parkingLots sortBy (_ emptyRate) lastOption) flatMap(_ park car)
  def pick(id: UUID): Option[Car] = parkingLots flatMap(_ pick id) headOption
  def canPark = parkingLots.exists(_ canPark)
}

class Manager[T <: Pickable with Parkable](val pickParkables: T*) {
  def park(car: Car): Option[UUID] = pickParkables.find(_ canPark) flatMap(_ park car)
  def pick(id: UUID): Option[Car] = pickParkables flatMap(_ pick id) headOption
}

class Car
