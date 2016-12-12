import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class ParkingLotSpec extends FlatSpec with Matchers {
  "parking lot" should "park a car and pick the same car" in {
    val lot = new ParkingLot()
    val car = new Car

    val token = lot park car
    token flatMap(lot pick) shouldBe Some(car)
  }

  it should "not pick a car with a token twice" in {
    val lot = new ParkingLot
    val car = new Car

    val token = lot park car

    token flatMap(lot pick) shouldBe Some(car)
    token flatMap(lot pick) shouldBe None
  }

  it should "not park car when it is full" in {
    val lot = new ParkingLot
    val car = new Car

    lot park new Car

    val token = lot park car
    token flatMap(lot pick) shouldBe None
  }

  it should "not pick car does not exist" in {
    val lot = new ParkingLot

    lot pick(UUID randomUUID) shouldBe None
  }
}
