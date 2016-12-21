import java.util.UUID
import scalaz._

trait Pickable {
  val pickParkable: Seq[Pickable]
  def pick(id: UUID): Option[Car] = pickParkable flatMap(_ pick id) headOption
}

trait Parkable {
  val pickParkable: Seq[Parkable]
  def park(car: Car): Option[UUID] = pickParkable find(_ canPark) flatMap(_ park car)
  def canPark: Boolean = pickParkable.exists(_ canPark)
}

class ParkingLot(slots: Int = 1) extends Pickable with Parkable {
  val pickParkable = Seq(this)

  var cars: List[(UUID, Car)] = List()

  override def park(car: Car): Option[UUID] = cars match {
    case cs if canPark =>
      val id = UUID.randomUUID()
      cars = (id, car) :: cs
      Some(id)
    case _             => None
  }

  override def pick(id: UUID): Option[Car] = {
    cars find (_._1 == id) match {
      case Some((id, car)) =>
        cars = cars filter {case (i, _) => i != id}
        Some(car)
      case _               => None
    }
  }

  override def canPark: Boolean = cars.length != slots
  def availableSlots: Int = slots - cars.length
  def emptyRate: Double = availableSlots.toDouble / slots.toDouble
}

class ParkingBoy(val pickParkable: ParkingLot*) extends Pickable with Parkable

class SmartParkingBoy(val pickParkable: ParkingLot*) extends Pickable with Parkable {
  override def park(car: Car): Option[UUID] = (pickParkable sortBy (_ availableSlots) lastOption) flatMap(_ park car)
}

class SuperParkingBoy(val pickParkable: ParkingLot*) extends Pickable with Parkable {
  override def park(car: Car): Option[UUID] = (pickParkable sortBy (_ emptyRate) lastOption) flatMap(_ park car)
}

class Manager[T <: Pickable with Parkable](val pickParkable: T*) extends Pickable with Parkable

class Car
