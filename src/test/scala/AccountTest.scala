import org.scalatest.flatspec.AnyFlatSpec

import java.time.{Clock, Instant, ZoneId}
import java.util.UUID
import org.scalatest.matchers.should.Matchers.shouldEqual

class AccountTest extends AnyFlatSpec {
  val clock: Clock = Clock.fixed(Instant.parse("2007-12-03T10:15:30.00Z"), ZoneId.of("UTC"))

  val account: Account = Account(generateUUIDFromString("Andrey"), clock = clock)

  def generateUUIDFromString(str: String): UUID = {
    UUID.nameUUIDFromBytes(str.getBytes())
  }
  "Account" should "no operations" in {
    account.showHistory() shouldEqual "                Time               Amount      Current Balance               Status"
  }

  it should "add money" in {
    val newAccount = account.transaction(500)
    newAccount.showHistory() shouldEqual
      "                Time               Amount      Current Balance               Status\n" +
      "            10:15:30                  500                  500              SUCCESS"

  }

  it should "add and send money" in {
    val newAccount = account
      .transaction(500)
      .transaction(-250)
    newAccount.showHistory() shouldEqual
      "                Time               Amount      Current Balance               Status\n" +
      "            10:15:30                  500                  500              SUCCESS\n" +
      "            10:15:30                 -250                  250              SUCCESS"
  }

  it should "send money fail" in {
    val newAccount = account
      .transaction(500)
      .transaction(-250)
      .transaction(-1000)
    newAccount.showHistory() shouldEqual
        "                Time               Amount      Current Balance               Status\n" +
        "            10:15:30                  500                  500              SUCCESS\n" +
        "            10:15:30                 -250                  250              SUCCESS\n" +
        "            10:15:30                -1000                  250               FAILED"
  }


}
