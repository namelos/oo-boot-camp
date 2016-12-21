import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class ManagerSpec extends FlatSpec with Matchers {
  "manager" should "park a car to a lot" in {
    val lot = new ParkingLot
    val manager = new Manager(lot)
    val car = new Car

    val token = manager park car

    token flatMap(lot pick) shouldBe Some(car)
  }

  it should "pick a car from a lot" in {
    val lot = new ParkingLot
    val car = new Car
    val token = lot park car
    val manager = new Manager(lot)

    token flatMap(manager pick) shouldBe Some(car)
  }


  it should "park a car to a parking boy" in {
    val boy = new ParkingBoy(new ParkingLot)
    val manager = new Manager(boy)
    val car = new Car

    val token = manager park car

    token flatMap(boy pick) shouldBe Some(car)
  }

  it should "pick car from parking boy" in {
    val boy = new ParkingBoy(new ParkingLot)
    val manager = new Manager(boy)
    val car = new Car

    val token = boy park car

    token flatMap(manager pick) shouldBe Some(car)
  }

  it should "park a car to a lot and a boy" in {
    val lot = new ParkingLot
    val boy = new ParkingBoy(new ParkingLot)
    val car = new Car
    val manager = new Manager(lot, boy)

    val token = manager park car

    val carMayFromLot = token flatMap(lot pick)
    val carMayFromBoy = token flatMap(boy pick)

    (carMayFromLot ++ carMayFromBoy).headOption shouldBe Some(car)
  }

  it should "pick a car from a lot and a boy" in {
    val lot = new ParkingLot
    val carInLot = new Car
    val lotToken = lot park carInLot

    val boy = new ParkingBoy(new ParkingLot)
    val carInBoy = new Car
    val boyToken = boy park carInBoy

    val manager = new Manager(lot, boy)

    lotToken flatMap(manager pick) shouldBe Some(carInLot)
    boyToken flatMap(manager pick) shouldBe Some(carInBoy)
  }
}

