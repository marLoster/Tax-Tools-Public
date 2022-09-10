import scala.math.Ordered

class Money protected (val whole : Int, val cents : Int) extends Ordered[Money] {

  def compare(that: Money): Int = {
    if (whole - that.whole != 0) whole - that.whole else cents - that.cents
  }

  def +(that: Money): Money = Money(this.whole + that.whole, this.cents + that.cents)
  def -(that: Money): Money = Money(this.whole - that.whole, this.cents - that.cents)

  def +(x: Int): Money = Money(this.whole + x, this.cents)
  def -(x: Int): Money = Money(this.whole - x, this.cents)

  def +(x: Double): Money = {
    val cents = ((x - x.toInt)*100).toInt
    Money(this.whole + x.toInt, this.cents + cents)
  }
  def -(x: Double): Money = {
    val cents = ((x - x.toInt)*100).toInt
    Money(this.whole - x.toInt, this.cents - cents)
  }

  def *(x : Double): Money = Money((whole * x).toInt + ((cents * x) / 100).toInt, (cents * x).toInt + (((whole * x) - (whole * x).toInt) * 100).toInt)
  def /(x : Double): Money = Money((whole/x).toInt, (cents/x).toInt + (((whole/x) - (whole/x).toInt)*100).toInt)

  def zero(): Money = Money()

  override def toString: String = whole.toString + "." + (cents/10).toString + (cents%10).toString

}

object Money {

  def apply(whole : Int = 0, cents : Int = 0): Money = {
    if (cents >= 0) new Money(whole + cents/100, cents%100)
    else new Money(whole + cents/100 - 1,cents%100 + 100)
  }

}