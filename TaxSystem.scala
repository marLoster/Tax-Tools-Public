
class TaxSystem(val stages : List[List[Tax]] = List[List[Tax]](List[Tax]())) {

  def calculate(money: Money): (Money, Money) = {
    val income = money

    @scala.annotation.tailrec
    def eval(money: (Money,Money), steps: List[List[Tax]]): (Money, Money) = {
      val current_base = money._1

      @scala.annotation.tailrec
      def evaluate(money: (Money,Money), steps: List[Tax]): (Money, Money) = {
        if (steps.isEmpty) money else evaluate(steps.head.evaluate(money,current_base,income), steps.tail)
      }
      if (steps.isEmpty) money else eval(evaluate(money, steps.head),steps.tail)
    }
    eval((money,money.zero()), stages)
  }

  def addTax(tax: Tax) : TaxSystem = {
    new TaxSystem(stages.dropRight(1) :+ (stages.last :+ tax))
  }

  def addStep() : TaxSystem = {
    new TaxSystem(stages :+ List[Tax]())
  }


}
