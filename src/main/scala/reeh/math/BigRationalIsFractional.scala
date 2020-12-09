package reeh.math

trait BigRationalIsFractional extends Fractional[BigRational] {
  override def div(x: BigRational, y: BigRational): BigRational = x / y

  override def plus(x: BigRational, y: BigRational): BigRational = x + y

  override def minus(x: BigRational, y: BigRational): BigRational = x - y

  override def times(x: BigRational, y: BigRational): BigRational = x * y

  override def negate(x: BigRational): BigRational = -x

  override def fromInt(x: Int): BigRational = x

  override def parseString(str: String): Option[BigRational] = scala.util.Try(BigRational(str)).toOption
  
  override def toInt(x: BigRational): Int = x.toInt

  override def toLong(x: BigRational): Long = x.toLong

  override def toFloat(x: BigRational): Float = x.toFloat

  override def toDouble(x: BigRational): Double = x.toDouble

}

implicit object BigRationalIsFractional extends BigRationalIsFractional with BigRationalOrdering {
}
