import org.scalatest.funsuite.AnyFunSuite

class DataProcessorTest extends AnyFunSuite {

  test("processData should return unchanged map if incorrect int is provided") {
    val map: Map[String, (Int, Int, Int, Long, Int)] = Map()
    val newItem: (String, String) = ("a", "bug")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map()

    assertResult(expectedOutput)(processedData)
  }

  test("processData should add new element if key in map not exists") {
    val map: Map[String, (Int, Int, Int, Long, Int)] = Map()
    val newItem: (String, String) = ("a", "5")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map("a" -> ((5, 5, 0, 5, 1)))

    assertResult(expectedOutput)(processedData)
  }

  test("processData should update element if key in map exists") {
    val map: Map[String, (Int, Int, Int, Long, Int)] = Map("a" -> ((10, 10, 0, 10, 1)))
    val newItem: (String, String) = ("a", "15")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map("a" -> ((10, 15, 0, 25, 2)))

    assertResult(expectedOutput)(processedData)
  }

  test("processData should increment third number in tuple if NaN as second value is provided") {
    val map: Map[String, (Int, Int, Int, Long, Int)] = Map("a" -> ((20, 20, 0, 20, 2)))
    val newItem: (String, String) = ("a", "NaN")
    val processedData = DataProcessor.processData(map, newItem)
    val expectedOutput = Map("a" -> ((20, 20, 1, 20, 2)))

    assertResult(expectedOutput)(processedData)
  }

  test("testCalculateAvg should return NaN on count = 0") {
    val avg = DataProcessor.calculateAvg(1, 0)

    assert(Double.NaN.equals(avg))
  }

  test("testCalculateAvg should calculate average correctly") {
    assertResult(6L.toDouble/2)(DataProcessor.calculateAvg(6L, 2))
    assertResult(200L.toDouble/75)(DataProcessor.calculateAvg(200L, 75))
    assertResult(3L.toDouble/2)(DataProcessor.calculateAvg(3L, 2))
  }

  test("testCalculateAvg should return NaN in case of division by 0") {
    assert(Double.NaN.equals(DataProcessor.calculateAvg(2, 0)))
  }

  test("addAvgToTuple should add average to all tuples in map") {
    val map = Map("a" -> (1, 25, 5, 100L, 25), "b" -> (15, 35, 10, 200L, 75))

    val aAvg = map("a")._4.toDouble / map("a")._5
    val bAvg = map("b")._4.toDouble / map("b")._5

    val mapWithAvg = DataProcessor.addAvgToTuple(map)

    assertResult(aAvg)(mapWithAvg("a")._6)
    assertResult(bAvg)(mapWithAvg("b")._6)
  }

}
