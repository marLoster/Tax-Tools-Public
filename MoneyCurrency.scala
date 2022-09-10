import sttp.client3._
import io.circe._
import io.circe.parser._
import scala.util.Try

class MoneyCurrency protected (whole: Int = 0, cents: Int = 0, curr: Currency ) extends Money(whole,cents)
{

  private val code = curr.code

  def convert(newCurr: Currency): MoneyCurrency = {
    val newCode = newCurr.code
    val w = whole.toString
    val c = cents.toString
    val key = ""
    val link = uri"https://api.getgeoapi.com/v2/currency/convert?api_key=$key&from=$code&to=$newCode&amount=$w.$c&format=json"

    val backend = HttpClientSyncBackend()

    val response = basicRequest.get(link).response(asString).send(backend)

    response.body match {
      case Left(error) =>
        println(error)
        throw new ConvertCurrencyException
      case Right(data) =>
        val cursor: HCursor = parse(data).getOrElse(Json.Null).hcursor
        val res = cursor.downField("rates").downField(newCode).downField("rate_for_amount").as[String].getOrElse(None).toString.split('.')
        MoneyCurrency(newCurr, res(0).toInt, res(1).slice(0, 2).toInt)
    }
  }

  def toMoney: Money = Money(whole, cents)

  override def toString: String = toMoney.toString + " " + code

  def +(that: MoneyCurrency): MoneyCurrency = MoneyCurrency(curr, toMoney + that.convert(curr).toMoney)
  def -(that: MoneyCurrency): MoneyCurrency = MoneyCurrency(curr, toMoney - that.convert(curr).toMoney)

  override def +(that: Money): MoneyCurrency = MoneyCurrency(curr, toMoney + that)
  override def -(that: Money): MoneyCurrency = MoneyCurrency(curr, toMoney - that)

  override def +(x: Int): MoneyCurrency = MoneyCurrency(curr, toMoney + x)
  override def -(x: Int): MoneyCurrency = MoneyCurrency(curr, toMoney - x)

  override def /(x: Double): MoneyCurrency = MoneyCurrency(curr, toMoney / x)
  override def *(x: Double): MoneyCurrency = MoneyCurrency(curr, toMoney * x)
  override def +(x: Double): MoneyCurrency = MoneyCurrency(curr, toMoney + x)
  override def -(x: Double): MoneyCurrency = MoneyCurrency(curr, toMoney - x)

  override def zero(): MoneyCurrency = MoneyCurrency(curr)

  override def compare(that: Money): Int = {
    val res = Try {
      val second = that.asInstanceOf[MoneyCurrency].convert(this.curr)
      this.toMoney.compare(second)
    }
    if (res.isSuccess) res.get else super.compare(that)
  }
}

object MoneyCurrency {

  def apply(code: Currency, whole: Int = 0, cents: Int = 0): MoneyCurrency =
    new MoneyCurrency(whole + cents / 100, cents % 100, code)

  def apply(curr: Currency, money: Money): MoneyCurrency =
    new MoneyCurrency(money.whole, money.cents, curr)

  def convertTuple(tuple: (Money,Money), curr: Currency): (MoneyCurrency,MoneyCurrency) = {
    val res = Try {
      (tuple._1.asInstanceOf[MoneyCurrency].convert(curr), tuple._2.asInstanceOf[MoneyCurrency].convert(curr))
    }
    if (res.isSuccess) res.get else (MoneyCurrency(curr,tuple._1),MoneyCurrency(curr,tuple._2))
  }
}


