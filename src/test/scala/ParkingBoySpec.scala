import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class ParkingBoySpec extends FlatSpec with Matchers {
  "parking boy" should "park a car" in {
    val lot = new ParkingLot
    val boy = new ParkingBoy(lot)
    val car = new Car

    val token = boy park car

    token flatMap(lot pick) shouldBe Some(car)
  }

  it should "picks car" in {
    val car = new Car
    val lotWithACar = new ParkingLot
    val token = lotWithACar park car
    val boy = new ParkingBoy(lotWithACar)

    token flatMap(boy pick) shouldBe Some(car)
  }

  it should "not pick a car does not exist" in {
    val lot = new ParkingLot
    val boy = new ParkingBoy(lot)

    boy pick (UUID randomUUID) shouldBe None
  }

  it should "park a car in the second lot when there are two lot " +
    "and the first lot is full while the second is not full" in {
    val fullLot = new ParkingLot
    val notFullLot = new ParkingLot
    val boy = new ParkingBoy(fullLot, notFullLot)
    val car = new Car
    boy park new Car

    val token = boy park car

    token flatMap(notFullLot pick) shouldBe Some(car)
  }

  it should "not park car when all the parking lots are full" in {
    val fullLot1 = new ParkingLot
    val fullLot2 = new ParkingLot
    fullLot1 park new Car
    fullLot2 park new Car
    val boy = new ParkingBoy(fullLot1, fullLot2)
    val car = new Car

    val token = boy park car

    token flatMap(fullLot1 pick) shouldBe None
    token flatMap(fullLot2 pick) shouldBe None
  }
}
