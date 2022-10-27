import Range.*
import org.scalacheck.Prop.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldEqual


class RangeSpec extends AnyFlatSpec {
  "Ranges" should "create empty range" in {
    Range.empty.length shouldEqual 0
  }

  it should "create with -->" in {
    2 --> 3 shouldEqual Range(2, 3)
  }

  it should "create nonempty interval Range(2,3) = [2, 3]" in {
    (2 --> 3).length shouldEqual 1
  }

  it should "create nonempty interval Range(3, 2) = [2, 3]" in {
    (3 --> 2).length shouldEqual 1
  }

  it should "intersections of intersecting intervals" in {
    2 --> 5 intersect 4 --> 7 shouldEqual 4 --> 5
  }

  it should "intersection with a nested interval" in {
    2 --> 5 intersect 3 --> 5 shouldEqual 3 --> 5
    2 --> 5 intersect 3 --> 4 shouldEqual 3 --> 4
  }

  it should "intersection of non-intersecting" in {
    2 --> 3 intersect 4 --> 5 shouldEqual Range.empty
  }

  it should "intersection with empty interval" in {
    2 --> 5 intersect Range.empty shouldEqual Range.empty
    Range.empty intersect 2 --> 5 shouldEqual Range.empty
    Range.empty intersect Range.empty shouldEqual Range.empty
  }

  it should "union of intersecting intervals" in {
    2 --> 5 union 4 --> 7 shouldEqual Some(2 --> 7)
  }

  it should "union of non-intersecting intervals" in {
    2 --> 5 union 6 --> 7 shouldEqual None
  }

  it should "check if the interval is empty" in {
    Range.empty.isEmpty shouldEqual true
    (1 --> 2).isEmpty shouldEqual false
  }

  it should "is contained in the interval" in {
    1 --> 2 contains 1 shouldEqual true
    1 --> 2 contains 3 shouldEqual false
  }

  it should "do the intervals intersect" in {
    1 --> 3 isIntersect 1 --> 2 shouldEqual true
    1 --> 3 isIntersect -1 --> 0 shouldEqual false
  }

  it should "do the interval contains in another interval" in {
    1 --> 2 contains 0 --> 5 shouldEqual true
    1 --> 2 contains 2 --> 3 shouldEqual false
  }

  it should "show all points in interval" in {
    (-1 --> 5).allPoints shouldEqual List(-1, 0, 1, 2, 3, 4, 5)
  }

  it should "calc minimum" in {
    (-1 --> 5 intersect -2 --> 4).minimum shouldEqual Some(-1)
  }

  it should "calc maximum" in {
    (-1 --> 5 intersect -2 --> 4).maximum shouldEqual Some(4)
  }

  it should "calc minimum empty list" in {
    Range.empty.minimum shouldEqual None
  }

  it should "calc maximum empty list" in {
    Range.empty.maximum shouldEqual None
  }

  it should "interval to string" in {
    (-1 --> 3).toString shouldEqual "[-1, 3]"
  }

  it should "empty interval to string" in {
    Range.empty.toString shouldEqual "empty"
  }


}
