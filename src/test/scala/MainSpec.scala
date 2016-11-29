import org.scalatest.{FlatSpec, Matchers}

class MainSpec extends FlatSpec with Matchers {
  "compare" should "return the nearer parking" in {
    Main.compare(ParkingLot(100), ParkingLot(200)) should
      be (ParkingLot(100))
  }
}
