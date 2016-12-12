import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class SmartParkingBoySpec extends FlatSpec with Matchers {
  "smart parking boy" should "park a car in the lot which has most empty slots" in {
    val lotWith2EmptySlots = new ParkingLot(2)
    val lotWith1EmptySlots = new ParkingLot(100)
    1 to 99 foreach (_ => lotWith1EmptySlots.park(new Car))
    val boy = new SmartParkingBoy(lotWith1EmptySlots, lotWith2EmptySlots)
    val car = new Car

    val token = boy park car
    token flatMap(lotWith2EmptySlots pick) shouldBe Some(car)
  }

  it should "picks car" in {
    val car = new Car
    val lotWithACar = new ParkingLot
    val token = lotWithACar park car
    val boy = new SmartParkingBoy(lotWithACar)

    token flatMap(boy pick) shouldBe Some(car)
  }

  it should "not pick a car does not exist" in {
    val lot = new ParkingLot
    val boy = new SmartParkingBoy(lot)

    boy pick (UUID randomUUID) shouldBe None
  }

  it should "park either one of two lots when there are same empty slots" in {
    val lotWith1Slot = new ParkingLot
    val anotherLotWith1Slot = new ParkingLot
    val boy = new SmartParkingBoy(lotWith1Slot, anotherLotWith1Slot)
    val car = new Car

    val token = boy park car

    val maybeCar1 = token flatMap(lotWith1Slot pick)
    val maybeCar2 = token flatMap(anotherLotWith1Slot pick)
    (maybeCar1 ++ maybeCar2).head shouldBe car
  }

  it should "not park car when parking lots are all full" in {
    val fullParkingLot = new ParkingLot
    fullParkingLot park new Car
    val boy = new SmartParkingBoy(fullParkingLot)
    val car = new Car

    val token = boy park car

    token flatMap(fullParkingLot pick) shouldBe None
  }
}
