import org.scalatest.{FlatSpec, Matchers}

class ParkingLotSpec extends FlatSpec with Matchers {
  "parking lot" should "park a car and pick the same car" in {
    val parkingLot = new ParkingLot
    val car = new Car

    parkingLot.park(car)

    parkingLot.pick shouldBe Some(car)
  }

  it should "not park car when it is full" in {
    val parkingLot = new ParkingLot
    val car = new Car

    parkingLot.park(new Car)
    parkingLot.park(car)

    parkingLot.pick should not be car
  }

  it should "not pick car when it is empty" in {
    val parkingLot = new ParkingLot

    parkingLot.pick shouldBe None
  }
}

class ParkingBoySpec extends FlatSpec with Matchers {
  "parking boy" should "park a car" in {
    val lot = new ParkingLot
    val boy = new ParkingBoy(List(lot))
    val car = new Car

    boy.park(car)

    lot.pick shouldBe Some(car)
  }

  it should "park a car in the second lot when there are two lot " +
    "and the first lot is full while the second is not full" in {
    val lot1 = new ParkingLot
    val lot2 = new ParkingLot
    val boy = new ParkingBoy(List(lot1, lot2))
    val car = new Car

    boy.park(new Car)
    boy.park(car)

    lot2.pick shouldBe Some(car)
  }

  it should "not park car when all the parking lots are full" in {
    val lot1 = new ParkingLot
    val lot2 = new ParkingLot
    lot1.park(new Car)
    lot2.park(new Car)
    val boy = new ParkingBoy(List(lot1, lot2))
    val car = new Car

    boy.park(car)

    lot2.pick should not be Some(car)
  }
}
