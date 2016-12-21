import java.util.UUID
import scalaz._

trait Pickable {
  def pick(id: UUID): Option[Car]
}

trait Parkable {
  def findLot: Option[ParkingLot]
  def park(car: Car): Option[UUID] = findLot flatMap (_ park car)
}

class ParkingLot(slots: Int = 1) extends Pickable with Parkable {
  var cars: List[(UUID, Car)] = List()

  override def park(car: Car): Option[UUID] = cars match {
    case cs if notFull =>
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

  def notFull: Boolean = cars.length != slots
  def availableSlots: Int = slots - cars.length
  def emptyRate: Double = availableSlots.toDouble / slots.toDouble
  def findLot: Option[ParkingLot] = Some(this)
}

class ParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def findLot: Option[ParkingLot] = parkingLots find(_ notFull)
  def pick(id: UUID): Option[Car] = parkingLots flatMap(_ pick id) headOption
}

class SmartParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def findLot: Option[ParkingLot] = Some(parkingLots sortBy (_ availableSlots) last)
  def pick(id: UUID): Option[Car] = parkingLots flatMap(_ pick id) headOption
}

class SuperParkingBoy(val parkingLots: ParkingLot*) extends Pickable with Parkable {
  def findLot: Option[ParkingLot] = Some(parkingLots sortBy (_ emptyRate) last)
  def pick(id: UUID): Option[Car] = parkingLots flatMap(_ pick id) headOption
}

class Car
