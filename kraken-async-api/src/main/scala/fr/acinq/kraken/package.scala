package fr.acinq

/**
 * Created by fabrice on 8/3/2015.
 */
package object kraken {
  type Ticker = Map[String, CurrencyPairTicker]
  type Balance = Map[String, Double]
}
