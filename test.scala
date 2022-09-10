import MoneyCurrency._
import scala.util.{Failure, Try}

object test extends App {

  println("10 zl + 2 euro = " + (MoneyCurrency(Currency("PLN"),10) + MoneyCurrency(Currency("EUR"),2)))
  println("10 euro - 2 zl = " + (MoneyCurrency(Currency("EUR"),10) - MoneyCurrency(Currency("PLN"),2)))

  println("-----")

  println("10 zl + 2.14 = " + (MoneyCurrency(Currency("PLN"),10) + 2.14))
  println("10 zl - 1.01 = " + (MoneyCurrency(Currency("PLN"),10) - 1.01))
  println("10 zl * 2.525 = " + (MoneyCurrency(Currency("PLN"),10) * 2.525))
  println("10 zl / 2 = " + (MoneyCurrency(Currency("PLN"),10) / 2))

  println("-----")

  println("10 zl > 10 euro - " + (MoneyCurrency(Currency("PLN"),10) > MoneyCurrency(Currency("EUR"),10)))
  println("10 zl <= 10 euro - " + (MoneyCurrency(Currency("PLN"),10) <= MoneyCurrency(Currency("EUR"),10)))

  println("-----")

  val income = MoneyCurrency(Currency("GBP"),70000)

  val taxSystem = new TaxSystem().addTax(new PercentTax((MoneyCurrency(Currency("EUR"),10000),0.20),(MoneyCurrency(Currency("EUR"),100000),0.32)))
  val taxSystem2 = new TaxSystem().addTax(new PercentTax((MoneyCurrency(Currency("PLN")),0.08)))
                                 .addTax(new PercentTax((MoneyCurrency(Currency("PLN")),0.02)))
                                 .addStep()
                                 .addTax(new PercentTax((MoneyCurrency(Currency("PLN")),0.10)))
  val taxSystem3 = new TaxSystem().addTax(new PercentTax((MoneyCurrency(Currency("EUR"),20000),0.32)))
  println("tax 1: " + taxSystem.calculate(income))
  println("tax 2: " + taxSystem2.calculate(income))
  println("tax 3: " + taxSystem3.calculate(income))
  println("tax 3: " + convertTuple(taxSystem3.calculate(income),Currency("EUR")))

  println("-----")
  println("exception: ")
  val invalidCurrency = Try {
    Currency("abcde")
  }
  println("isFailure: " + invalidCurrency.isFailure)
  println("-----")
  val incomes = List[MoneyCurrency](MoneyCurrency(Currency("EUR"),200),MoneyCurrency(Currency("EUR"),100),MoneyCurrency(Currency("EUR"),50))

  val taxSystem4 = new TaxSystem().addTax(new ConstConditionalTax(_ => true, MoneyCurrency(Currency("EUR"),100)))

  println(taxSystem4.calculate(incomes(0)))
  println(taxSystem4.calculate(incomes(1)))
  println(taxSystem4.calculate(incomes(2)))

  val taxSystem5 = new TaxSystem().addTax(new PercentTax((MoneyCurrency(Currency("EUR")),0.1)))
                                  .addTax(new TaxRelief(MoneyCurrency(Currency("EUR"),100)))
  println("-----")
  println(taxSystem5.calculate(MoneyCurrency(Currency("EUR"),10000)))
  println(taxSystem5.calculate(incomes(0)))
  println(taxSystem5.calculate(incomes(1)))
  println(taxSystem5.calculate(MoneyCurrency(Currency("EUR"))))
}

