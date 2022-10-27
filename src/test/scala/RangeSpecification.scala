import Range.-->
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, oneOf}

object RangeSpecification extends Properties("Range") {

  import Prop.forAll

  val genEmptyRange: Gen[Range] = const(Range.empty)

  val genNonEmptyRange: Gen[Range] = for {
    a <- arbitrary[Int]
    b <- arbitrary[Int]
  } yield a --> b

  def genRange: Gen[Range] = oneOf(genEmptyRange, genNonEmptyRange)

  implicit lazy val arbRange: Arbitrary[Range] = Arbitrary(genRange)

  property("commutative law for intersection") =
    forAll { (a: Range, b: Range) =>
      (a intersect b) == (b intersect a)
    }

  property("associativity law for intersection") =
    forAll { (a: Range, b: Range, c: Range) =>
      ((a intersect b) intersect c) == (a intersect (b intersect c))
  }

  property("commutative law for union") =
    forAll { (a: Range, b: Range) =>
      (a union b) == (b union a)
    }


}
