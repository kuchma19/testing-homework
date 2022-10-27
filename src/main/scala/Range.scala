import scala.annotation.targetName

sealed trait Range {
  def length: Int

  def intersect(other: Range): Range

  def union(other: Range): Option[Range]

  val isEmpty: Boolean

  def contains(i: Int): Boolean

  def contains(other: Range): Boolean

  def isIntersect(other: Range): Boolean

  def allPoints: List[Int]

  val minimum: Option[Int]

  val maximum: Option[Int]
}

object Range {

  private case object Empty extends Range {
    override def length: Int = 0

    override def intersect(other: Range): Range = this

    override def union(other: Range): Option[Range] = Some(other)

    override val isEmpty: Boolean = true

    override def contains(i: Int): Boolean = false

    override def contains(other: Range): Boolean = false

    override def isIntersect(other: Range): Boolean = false

    override def allPoints: List[Int] = List.empty

    override val minimum: Option[Int] = None

    override val maximum: Option[Int] = None

    override def toString: String = "empty"
  }

  private case class NonEmpty(_minimum: Int, _maximum: Int) extends Range {
    override def length: Int = _maximum - _minimum

    override def intersect(other: Range): Range =
      other match
        case Empty => Empty
        case NonEmpty(otherMinimum, otherMaximum) =>
          val currentMinimum = _minimum max otherMinimum
          val currentMaximum = _maximum min otherMaximum
          if (currentMinimum <= currentMaximum)
            Range(currentMinimum, currentMaximum)
          else Empty

    override def union(other: Range): Option[Range] =
      other match
        case Empty => Some(this)
        case NonEmpty(otherMinimum, otherMaximum) =>
          if (!isIntersect(other)) None
          else
            val currentMinimum = _minimum min otherMinimum
            val currentMaximum = _maximum max otherMaximum
            Some(Range(currentMinimum, currentMaximum))

    override val isEmpty: Boolean = false

    override def contains(i: Int): Boolean = _minimum <= i && i <= _maximum

    override def isIntersect(other: Range): Boolean = !(this intersect other).isEmpty

    override def contains(other: Range): Boolean =
      other match
        case Empty => false
        case NonEmpty(otherMinimum, otherMaximum) =>
          otherMinimum <= _minimum && _maximum <= otherMaximum

    override def allPoints: List[Int] = (_minimum to _maximum).toList

    override val minimum: Option[Int] = Some(_minimum)

    override val maximum: Option[Int] = Some(_maximum)

    override def toString: String = s"[$_minimum, $_maximum]"
  }

  val empty: Range = Empty

  def apply(a: Int, b: Int): Range =
    if (a < b) NonEmpty(a, b)
    else NonEmpty(b, a)

  extension (i: Int)
    @targetName("Range.apply")
    def -->(j: Int) = Range(i, j)
}