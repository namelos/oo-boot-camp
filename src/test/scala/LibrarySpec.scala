import org.scalatest.{FlatSpec, Matchers}

class LibrarySpec extends FlatSpec with Matchers {
  "Library" should "return true" in {
    assert(Set.empty.size == 0)
  }
}
