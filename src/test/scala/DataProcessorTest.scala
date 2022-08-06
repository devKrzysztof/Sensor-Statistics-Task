import org.scalatest.funsuite.AnyFunSuite

class DataProcessorTest extends AnyFunSuite {

  test("processData should return unchanged map if incorrect int is provided") {
    val map: Map[String, SensorStat] = Map()
    val newItem: SensorDataRow = SensorDataRow("a", "bug")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map()

    assertResult(expectedOutput)(processedData)
  }

  test("processData should add new element if key in map not exists") {
    val map: Map[String, SensorStat] = Map()
    val newItem: SensorDataRow = SensorDataRow("a", "5")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map("a" -> SensorStat(5, 5, 0, 5, 1, 0))

    assertResult(expectedOutput)(processedData)
  }

  test("processData should update element if key in map exists") {
    val map: Map[String, SensorStat] = Map("a" -> SensorStat(10, 10, 0, 10, 1, 0))
    val newItem: SensorDataRow = SensorDataRow("a", "15")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map("a" -> SensorStat(10, 15, 0, 25, 2, 0))

    assertResult(expectedOutput)(processedData)
  }

  test("processData should increment third number in tuple if NaN as second value is provided") {
    val map: Map[String, SensorStat] = Map("a" -> SensorStat(20, 20, 0, 20, 2, 0))
    val newItem: SensorDataRow = SensorDataRow("a", "NaN")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map("a" -> SensorStat(20, 20, 1, 20, 2, 0))

    assertResult(expectedOutput)(processedData)
  }

  test("testCalculateAvg should return NaN on count = 0") {
    val avg = DataProcessor.calculateAvg(1, 0)

    assert(Double.NaN.equals(avg))
  }

  test("testCalculateAvg should calculate average correctly") {
    assertResult(6L.toDouble / 2)(DataProcessor.calculateAvg(6L, 2))
    assertResult(200L.toDouble / 75)(DataProcessor.calculateAvg(200L, 75))
    assertResult(3L.toDouble / 2)(DataProcessor.calculateAvg(3L, 2))
  }

  test("testCalculateAvg should return NaN in case of division by 0") {
    assert(Double.NaN.equals(DataProcessor.calculateAvg(2, 0)))
  }

  test("calculateAvgInSensorStat should add average to all case classes in map") {
    val map = Map("a" -> SensorStat(1, 25, 5, 100L, 25, 0), "b" -> SensorStat(15, 35, 10, 200L, 75, 0))

    val aAvg = map("a").sum.toDouble / map("a").count
    val bAvg = map("b").sum.toDouble / map("b").count

    val mapWithAvg = DataProcessor.calculateAvgInSensorStat(map)

    assertResult(aAvg)(mapWithAvg("a").avg)
    assertResult(bAvg)(mapWithAvg("b").avg)
  }

}
