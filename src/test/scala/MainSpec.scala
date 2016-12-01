import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class MainSpec extends FlatSpec with Matchers {
  "parking lot" should "put in car and withdraw the same car" in {
    val parkingLot = new ParkingLot
    val car1 = new Car
    val car2 = new Car

    val token1 = parkingLot.park(car1)
    val token2 = parkingLot.park(car2)

    parkingLot.withdraw(token2) shouldBe car2
    parkingLot.withdraw(token1) shouldBe car1
  }

  it should "throw error when withdrawing with a token does not exist" in {
    val parkingLot = new ParkingLot

    an [Exception] shouldBe thrownBy { parkingLot.withdraw(UUID.randomUUID()) }
  }

  it should "throw error when no there is no place to park" in {
    val parkingLot = new ParkingLot(1)

    parkingLot.park(new Car)

    an [Exception] shouldBe thrownBy {
      parkingLot.park(new Car)
    }
  }

  it should "return the amount empty slots" in {
    val parkingLot = new ParkingLot

    parkingLot.emptySlots shouldBe 4
    parkingLot.park(new Car)
    parkingLot.emptySlots shouldBe 3
    1 to 3 foreach { _ => parkingLot.park(new Car) }
    parkingLot.emptySlots shouldBe 0
  }
}
