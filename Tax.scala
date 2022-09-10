abstract class Tax {
  def evaluate(money: (Money, Money)) : (Money,Money) = evaluate(money, money._1, money._1)
  def evaluate(money: Money) : (Money, Money) = evaluate((money,money.zero()))
  def evaluate(money: (Money, Money), base: Money, income: Money) : (Money, Money)
}
