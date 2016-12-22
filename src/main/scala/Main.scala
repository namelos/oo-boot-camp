import java.util.UUID
import scalaz._

trait Pickable {
  val pickParkable: Seq[Pickable]
  def pick(id: UUID): Option[Car] = pickParkable flatMap(_ pick id) headOption
}

trait Parkable {
  val pickParkable: Seq[Parkable]
  def park(car: Car): Option[UUID] = pickParkable.find(_.canPark).flatMap(_ park car)
  def canPark: Boolean = pickParkable.exists(_ canPark)
}

trait Reportable {
  val pickParkable: Seq[Reportable]

  def slotsAmount: Int = pickParkable.map(_.slotsAmount).sum
  def carsAmount: Int = pickParkable.map(_.carsAmount).sum

//  def slotsAmount: Int
//  def carsAmount: Int
  def report: String
}

class ParkingLot(slots: Int = 1) extends Pickable with Parkable with Reportable {
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

  override def canPark: Boolean = carsAmount != slots
  def availableSlots: Int = slots - carsAmount
  def emptyRate: Double = availableSlots.toDouble / slots.toDouble
  override def carsAmount = cars.length
  override def slotsAmount = slots
  override def report = {
    val result = s"p $carsAmount $slotsAmount\n"
    println(result)
    result
  }
}

class ParkingBoy(val pickParkable: ParkingLot*) extends Pickable with Parkable with Reportable {
  def report: String = s"b $carsAmount $slotsAmount" + pickParkable.map(_ report).mkString("\n    ", "", "")
}

class SmartParkingBoy(val pickParkable: ParkingLot*) extends Pickable with Parkable with Reportable {
  override def park(car: Car): Option[UUID] = (pickParkable sortBy (_ availableSlots) lastOption) flatMap(_ park car)
  def report: String = s"b $carsAmount $slotsAmount" + pickParkable.map(_ report).mkString("\n    ", "", "")
}

class SuperParkingBoy(val pickParkable: ParkingLot*) extends Pickable with Parkable with Reportable {
  override def park(car: Car): Option[UUID] = (pickParkable sortBy (_ emptyRate) lastOption) flatMap(_ park car)
  def report: String = s"b $carsAmount $slotsAmount" + pickParkable.map(_ report).mkString("\n    ", "", "")
}

class Manager[T <: Pickable with Parkable with Reportable](val pickParkable: T*) extends Pickable with Parkable with Reportable {
  def report: String = {
    val result = s"m $carsAmount $slotsAmount" + pickParkable.map(_ report).mkString("\n  ", "  ", "")
    println(result)
    result
  }
}

class Car
