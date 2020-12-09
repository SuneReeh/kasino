package reeh.math

class BigRational private(val numerator: BigInt, val denominator: BigInt) extends scala.math.ScalaNumber with scala.math.ScalaNumericConversions with Serializable with Ordered[BigRational] {
  require(denominator.sign == 1)
  require(numerator.gcd(denominator) == 1)
  
  private lazy val computedHashCode = {
    if isWhole then
      numerator.hashCode()
    else
      numerator.hashCode() + 31 * denominator.hashCode()
  }
  override def hashCode(): Int = computedHashCode

  def equals (that: BigRational): Boolean = (numerator == that.numerator) && (denominator == that.denominator)

  override def equals (that: Any): Boolean = that match {
    case that: BigRational     => this equals that
    case that: BigInt         =>
      (isWhole) && (numerator == that)
    case that: BigDecimal    => 
      (BigDecimal(numerator) / BigDecimal(denominator)) == that
    /*case that: Double         =>
      !that.isInfinity && {
        val d = toDouble
        !d.isInfinity && d == that 
      }
    case that: Float          =>
      !that.isInfinity && {
        val f = toFloat
        !f.isInfinity && f == that 
      }*/
    case _                    => isValidInt && unifiedPrimitiveEquals(that)
  }

  def isWhole: Boolean = denominator == 1

  def underlying: Object = this

  override def isValidByte  = noArithmeticException(toByteExact)
  override def isValidShort = noArithmeticException(toShortExact)
  override def isValidChar  = isValidInt && toIntExact >= Char.MinValue && toIntExact <= Char.MaxValue
  override def isValidInt   = noArithmeticException(toIntExact)
  def isValidLong  = noArithmeticException(toLongExact)
  def isValidBigInt = isWhole

  private def noArithmeticException(body: => Unit): Boolean = {
    try   { body ; true }
    catch { case _: ArithmeticException => false }
  }

  def compare(that: BigRational): Int = (numerator * that.denominator).compare(that.numerator * denominator)


  infix def + (that: BigRational): BigRational = {
    val divisor: BigInt = denominator.gcd(that.denominator)
    BigRational(numerator * (that.denominator / divisor) + that.numerator * (denominator / divisor), (denominator / divisor) * that.denominator)
  }

  def unary_- : BigRational = BigRational(-numerator, denominator)

  infix def - (that: BigRational): BigRational = this + (-that)

  infix def * (that: BigRational): BigRational = BigRational(numerator * that.numerator, denominator * that.denominator)
  
  infix def / (that: BigRational): BigRational = BigRational(numerator * that.denominator, denominator * that.numerator)

  def min(that: BigRational): BigRational = this.compare(that) match {
    case x if x <= 0 => this
    case _           => that
  }

  def max (that: BigRational): BigRational = this.compare(that) match {
    case x if x >= 0 => this
    case _           => that
  }

  def pow (n: Int): BigRational = BigRational(numerator.pow(n), denominator.pow(n))

  def abs: BigRational = if (signum < 0) unary_- else this

  def signum: Int = numerator.signum

  def sign: BigRational = signum
  
  def floor: BigInt = numerator / denominator
  
  def ceil: BigInt = if isWhole then numerator else (numerator / denominator) +1

  def bigDecimalValue: BigDecimal = BigDecimal(numerator) / BigDecimal(denominator)
  
  def doubleValue: Double = bigDecimalValue.toDouble

  def floatValue: Float = bigDecimalValue.toFloat

  def intValue: Int = floor.toInt

  def longValue: Long = floor.toLong
  

  def toByteExact = {
    if isWhole && numerator <= Byte.MaxValue && numerator >= Byte.MinValue then
      numerator.toByte
    else
      throw new ArithmeticException
  }

  def toShortExact = {
    if isWhole && numerator <= Short.MaxValue && numerator >= Short.MinValue then
      numerator.toShort
    else
      throw new ArithmeticException
  }

  def toIntExact = {
    if isWhole && numerator <= Int.MaxValue && numerator >= Int.MinValue then
      numerator.toInt
    else
      throw new ArithmeticException
  }

  def toLongExact = {
    if isWhole && numerator <= Long.MaxValue && numerator >= Long.MinValue then
      numerator.toLong
    else
      throw new ArithmeticException
  }

  def toBigInt: BigInt = floor

  def toBigIntExact = {
    if isWhole then
      numerator
    else
      throw new ArithmeticException
  }
  
  override def toString: String = s"$numerator/$denominator"

}

object BigRational extends Serializable {
  /** Construct a [[BigRational]] as a fraction of `numerator` by `denominator`. */
  def apply(numerator: BigInt, denominator: BigInt): BigRational = {
    val divisor: BigInt = numerator.gcd(denominator)
    new BigRational((numerator / divisor) * denominator.sign, (denominator / divisor) * denominator.sign)
  }

  def apply(str: String): BigRational = {
    val split = str.split("/", 2)
    BigRational(BigInt(split(0)), BigInt(split(1)))
  }

  import scala.language.implicitConversions

  implicit def int2bigRational(i: Int): BigRational = BigRational(i, 1)

  implicit def long2bigRational(i: Long): BigRational = BigRational(i, 1)

  implicit def bigInt2bigRational(i: BigInt): BigRational = BigRational(i, 1)
  
  implicit def bigRational2bigDecimal(i: BigRational): BigDecimal = BigDecimal(i.numerator) / BigDecimal(i.denominator)

}