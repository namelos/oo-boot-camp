import java.util.UUID
import scalaz._
import Scalaz._

import org.scalatest.{FlatSpec, Matchers}

class ParkingLotSpec extends FlatSpec with Matchers {
  "parking lot" should "park a car and pick the same car" in {
    val parkingLot = new ParkingLot
    val car = new Car

    parkingLot park car flatMap (parkingLot pick) foreach (_ shouldBe car)
  }

  it should "not park car when it is full" in {
    val parkingLot = new ParkingLot
    val car = new Car

    parkingLot park new Car

    parkingLot park car shouldBe None
  }

  it should "not pick car when it is empty" in {
    val parkingLot = new ParkingLot

    parkingLot pick (UUID randomUUID) shouldBe None
  }
}

class ParkingBoySpec extends FlatSpec with Matchers {
  "parking boy" should "park a car" in {
    val lot = new ParkingLot
    val boy = new ParkingBoy(lot)
    val car = new Car

    boy park car flatMap (lot pick) map (_ shouldBe car)
  }

  it should "park a car in the second lot when there are two lot " +
    "and the first lot is full while the second is not full" in {
    val lot1 = new ParkingLot
    val lot2 = new ParkingLot
    val boy = new ParkingBoy(lot1, lot2)
    val car = new Car

    boy park new Car
    boy park car flatMap (lot2 pick) map (_ shouldBe car)
  }

  it should "not park car when all the parking lots are full" in {
    val lot1 = new ParkingLot
    val lot2 = new ParkingLot
    lot1 park new Car
    lot2 park new Car
    val boy = new ParkingBoy(lot1, lot2)
    val car = new Car

    boy park car shouldBe None
  }
}

class SmartParkingBoySpec extends FlatSpec with Matchers {
  "smart parking boy" should "park a car in the lot which has most empty slots" in {
    val lotWith2EmptySlots = new ParkingLot(2)
    val lotWith1EmptySlots = new ParkingLot(100)
    1 to 99 foreach (_ => lotWith1EmptySlots.park(new Car))
    val boy = new SmartParkingBoy(lotWith1EmptySlots, lotWith2EmptySlots)
    val car = new Car

    boy park car flatMap (lotWith2EmptySlots pick) map (_ shouldBe None)
  }
}
