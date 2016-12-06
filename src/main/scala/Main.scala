class ParkingLot(slots: Int = 1) {
  var cars: List[Car] = List()

  def park(car: Car) = cars match {
    case cs if !full => cars = car :: cs
      Some()
    case _           => None
  }

  def pick = cars match {
    case c :: cs if cars.nonEmpty => cars = cs
      Some(c)
    case _                        => None
  }

  def full = cars.length == slots
}

class ParkingBoy(parkingLots: List[ParkingLot]) {
  def park(car: Car) = parkingLots.find(lot => !lot.full) match {
    case Some(lot) => lot.park(car)
    case _         => None
  }
}

class Car
