import org.scalatest.funsuite.AnyFunSuite

class StatsPrinterTest extends AnyFunSuite {

  test("overallStats should calculate nan and total observations correctly") {
    val expectedResult = (1,2)
    val overallStats = StatsPrinter.overallStats(Seq(("s2", (88, 88, 0, 88, 1, 88.0)), ("s1", (10, 10, 1, 10, 1, 10.0))))

    assert(expectedResult == overallStats)
  }

}
