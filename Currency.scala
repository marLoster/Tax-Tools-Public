class Currency private (val code: String) {}

object Currency {
  val allowed_codes : List[String] = List[String]("STN","XAG","XAU","PLN","UGX","GGP","MWK","NAD","ALL","BHD","JEP","BWP","MRU",
                                          "BMD","MNT","FKP","PYG","AUD","KYD","RWF","WST","SHP","SOS","SSP","BIF","SEK",
                                          "CUC","BTN","MOP","XDR","IMP","INR","BYN","BOB","SRD","GEL","ZWL","EUR","BBD",
                                          "RSD","SDG","VND","VES","ZMW","KGS","HUF","BND","BAM","CVE","BGN","NOK","BRL",
                                          "JPY","HRK","HKD","ISK","IDR","KRW","KHR","XAF","CHF","MXN","PHP","RON","RUB",
                                          "SGD","AED","KWD","CAD","PKR","CLP","CNY","COP","AOA","KMF","CRC","CUP","GNF",
                                          "NZD","EGP","DJF","ANG","DOP","JOD","AZN","SVC","NGN","ERN","SZL","DKK","ETB",
                                          "FJD","XPF","GMD","AFN","GHS","GIP","GTQ","HNL","GYD","HTG","XCD","GBP","AMD",
                                          "IRR","JMD","IQD","KZT","KES","ILS","LYD","LSL","LBP","LRD","AWG","MKD","LAK",
                                          "MGA","ZAR","MDL","MVR","MUR","MMK","MAD","XOF","MZN","MYR","OMR","NIO","NPR",
                                          "PAB","PGK","PEN","ARS","SAR","QAR","SCR","SLL","LKR","SBD","VUV","USD","DZD",
                                          "BDT","BSD","BZD","CDF","UAH","YER","TMT","UZS","UYU","CZK","SYP","TJS","TWD",
                                          "TZS","TOP","TTD","THB","TRY","TND")

  def apply(text: String): Currency = {
    if (!allowed_codes.contains(text)) throw new InvalidCurrencyException()
    new Currency(text)
  }
}

