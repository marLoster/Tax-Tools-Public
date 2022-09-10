class PercentTax(rates: (Money,Double)*) extends Tax{

  private val SortedRates: Seq[(Money,Double)] = rates sortBy(_._1)

  override def evaluate(money: (Money,Money),base: Money, income: Money): (Money, Money) = {
    if (rates.isEmpty) throw new Exception

    @scala.annotation.tailrec
    def eval(amount: Money,current: (Money,Double), next: Seq[(Money,Double)], prev: Money): Money = {
      if (current._1 > amount) amount.zero()
      else if(next.isEmpty || next.head._1 > amount) prev + (amount - current._1) * current._2
      else eval(amount, next.head, next.tail, prev + (next.head._1 - current._1) * current._2)
    }

    val tax = eval(base,SortedRates.head,SortedRates.tail,SortedRates.head._1.zero())
    (if (money._1-tax >= Money()) money._1-tax else money._1.zero(), money._2 + tax)
  }
}
