object Main {
  def compare(park1: ParkingLot, park2: ParkingLot) =
    if (park1.distance < park2.distance)
      park1
    else
      park2
}

case class ParkingLot(distance: Int)