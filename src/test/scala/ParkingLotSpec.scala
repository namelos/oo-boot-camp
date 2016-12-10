import java.util.UUID
import scalaz._
import Scalaz._

import org.scalatest.{FlatSpec, Matchers}

class ParkingLotSpec extends FlatSpec with Matchers {
  "parking lot" should "park a car and pick the same car" in {
    val lot = new ParkingLot
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

  it should "not pick car when it is empty" in {
    val lot = new ParkingLot

    lot pick(UUID randomUUID) shouldBe None
  }
}

class ParkingBoySpec extends FlatSpec with Matchers {
  "parking boy" should "park a car" in {
    val lot = new ParkingLot
    val boy = new ParkingBoy(lot)
    val car = new Car

    val token = boy park car

    token flatMap(lot pick) shouldBe Some(car)
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

    token flatMap (fullLot1 pick) shouldBe None
    token flatMap (fullLot2 pick) shouldBe None
  }
}

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
