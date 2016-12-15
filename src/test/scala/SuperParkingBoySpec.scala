import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class SuperParkingBoySpec extends FlatSpec with Matchers {
  def givenLot(slots: Int, cars: Int) = {
    val lot = new ParkingLot(slots)
    0 to cars foreach(_ => lot park new Car)
    lot
  }

  "super parking boy" should "park a car in the lot which has highest empty rate" in {
    val lotWith2SlotsAnd1Car = givenLot(2, 1)
    val lotWith3SlotsAnd1Car = givenLot(3, 1)

    val boy = new SuperParkingBoy(lotWith2SlotsAnd1Car, lotWith3SlotsAnd1Car)
    val car = new Car
    val token = boy park car

    token flatMap(lotWith3SlotsAnd1Car pick) shouldBe Some(car)
  }

  it should "park a car with lot which has highest empty rate'" in {
    val lotWith2SlotsAnd1Car = new ParkingLot(2)
    lotWith2SlotsAnd1Car park new Car
    val lotWith3SlotsAnd2Car = new ParkingLot(3)
    lotWith3SlotsAnd2Car park new Car
    lotWith3SlotsAnd2Car park new Car

    val boy = new SuperParkingBoy(lotWith2SlotsAnd1Car, lotWith3SlotsAnd2Car)
    val car = new Car
    val token = boy park car

    token flatMap(lotWith2SlotsAnd1Car pick) shouldBe Some(car)
  }

  it should "picks car" in {
    val car = new Car
    val lotWithACar = new ParkingLot
    val token = lotWithACar park car
    val boy = new SuperParkingBoy(lotWithACar)

    token flatMap(boy pick) shouldBe Some(car)
  }

  it should "not pick a car does not exist" in {
    val lot = new ParkingLot
    val boy = new SuperParkingBoy(lot)

    boy pick (UUID randomUUID) shouldBe None
  }

  it should "park either one of two lots when there are same empty slots" in {
    val lotWith2SlotAnd1Car = new ParkingLot(2)
    lotWith2SlotAnd1Car park new Car
    val lotWith4SlotAnd2Car = new ParkingLot(4)
    lotWith4SlotAnd2Car park new Car
    lotWith4SlotAnd2Car park new Car
    println(lotWith2SlotAnd1Car.emptyRate)
    println(lotWith4SlotAnd2Car.emptyRate)
    val boy = new SuperParkingBoy(lotWith2SlotAnd1Car, lotWith4SlotAnd2Car)
    val car = new Car

    val token = boy park car

    val maybeCar1 = token flatMap(lotWith2SlotAnd1Car pick)
    val maybeCar2 = token flatMap(lotWith4SlotAnd2Car pick)
    (maybeCar1 ++ maybeCar2).head shouldBe car
  }

  it should "not park car when parking lots are all full" in {
    val fullParkingLot = new ParkingLot
    fullParkingLot park new Car
    val boy = new SuperParkingBoy(fullParkingLot)
    val car = new Car

    val token = boy park car

    token flatMap(fullParkingLot pick) shouldBe None
  }
}

