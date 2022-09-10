class TaxRelief(val amount: Money) extends Tax{
  override def evaluate(money: (Money, Money), base: Money, income: Money): (Money, Money) = {
    val returnedAmount = if (money._2 >= amount) amount else money._2
    (money._1 + returnedAmount,money._2 - returnedAmount)
  }
}
