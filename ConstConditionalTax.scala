class ConstConditionalTax(val check: Money => Boolean, val amount: Money) extends Tax {
  override def evaluate(money: (Money, Money), base: Money, income: Money): (Money, Money) = {
    val chargedAmount = if (money._1 >= amount) amount else money._1
    if (check(income)) (money._1 - chargedAmount,money._2 + chargedAmount) else money
  }
}
