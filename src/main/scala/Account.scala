import java.time.{Clock, LocalTime}
import java.util.UUID

enum StatusPayment:
  case SUCCESS, FAILED

case class Payment(time: LocalTime, amount: BigDecimal,
                   currentBalance: BigDecimal, status: StatusPayment)

case class Account(userId: UUID, clock: Clock = Clock.systemDefaultZone(),
              private val history: List[Payment] = List.empty) {

  private val sizeColumnInShowTable = 20

  private val balance: BigDecimal = history match
    case Nil => 0
    case x :: _ => x.currentBalance

  def transaction(amount: BigDecimal): Account = {
    val newBalanceWithoutCheck = balance + amount
    val (newBalance, newStatus) =
      if (newBalanceWithoutCheck < 0) (balance, StatusPayment.FAILED)
      else (newBalanceWithoutCheck, StatusPayment.SUCCESS)

    val currentPayment: Payment = Payment(
      time = LocalTime.now(clock),
      amount = amount,
      currentBalance = newBalance,
      status = newStatus
    )
    this.copy(history = currentPayment :: this.history)
  }

  private def showListString(strings: List[String]): String = {
    def stringToConstSymbols(str: String): String = {
      val len = str.length
      if (len > sizeColumnInShowTable) {
        str.take(sizeColumnInShowTable)
      } else {
        val x = sizeColumnInShowTable - len
        val strWithWhiteSpace = " ".repeat(x)
        strWithWhiteSpace + str
      }
    }
    strings.map(stringToConstSymbols).mkString(" ")
  }

  private def paymentToListString(payment: Payment): List[String] = {
    payment match
      case Payment(time, amount, currentBalance, status) =>
        List(time.toString, amount.toString(), currentBalance.toString(), status.toString)
  }

  def showHistory(): String = {
    val header = showListString(List("Time", "Amount", "Current Balance", "Status"))

    val rows = header :: history.map(paymentToListString).map(showListString).reverse

    rows.mkString("\n")
  }

}

object Account {
  def apply(userId: UUID): Account = new Account(userId)
}

