package fr.acinq.paymium

import java.util.Date

case class Error(errors: List[String])

case class Ticker(
                   high: Double,
                   low: Double,
                   volume: Double,
                   bid: Double,
                   ask: Double,
                   midpoint: Double,
                   vwap: Double,
                   at: Long,
                   price: Double,
                   variation: Double,
                   currency: String
                   )

case class UserInfo(
                     name: String,
                     locale: String,
                     balance_btc: Double,
                     locked_btc: Double,
                     balance_eur: Double,
                     locked_eur: Double
                     )

case class Order(
                  uuid: String,
                  amount: Option[Double],
                  currency_amount: Option[Double],
                  state: String,
                  btc_fee: Int,
                  currency_fee: Int,
                  updated_at: Date,
                  created_at: Date,
                  currency: String,
                  `type`: String,
                  traded_btc: Double,
                  traded_currency: Double,
                  direction: String,
                  price: Option[Double],
                  account_operations: List[AccountOperation]
                  )

case class AccountOperation(
                             uuid: String,
                             name: String,
                             amount: Double,
                             currency: String,
                             created_at: Date,
                             created_at_int: Long,
                             is_trading_account: Boolean
                             )
