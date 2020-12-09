package reeh.math

trait BigRationalOrdering extends Ordering[BigRational] {
  override def compare(x: BigRational, y: BigRational): Int = x.compare(y)
}

implicit object BigRationalOrdering extends BigRationalOrdering {
}