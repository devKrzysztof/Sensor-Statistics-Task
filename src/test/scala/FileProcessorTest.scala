import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class FileProcessorTest extends AnyFunSuite {
  private val absolutePath = new File("src/test/resources").getAbsolutePath + File.separator

  test("getCsvsInPath should read files list") {
    val filesList = FileProcessor.getCSVsInPath(new File("src/test/resources").listFiles)
    assert(Array(absolutePath + "leader-1.csv", absolutePath + "leader-2.csv").sameElements(filesList))
  }

  test("getFilesArray should return paths array of files inside directory") {
    assert(FileProcessor.getFilesArray("src/test/resources").isDefined)
    assert(FileProcessor.getFilesArray("///<|>test\\\\").isEmpty)
  }

  test("fileProcessor should process files correctly") {
    val processedFile = FileProcessor.fileProcessor(absolutePath + "leader-1.csv")
    val expectedResult = Map("s1" -> SensorStat(10, 10, 1, 10L, 1, 0), "s2" -> SensorStat(88, 88, 0, 88L, 1, 0))

    assertResult(expectedResult)(processedFile)
  }

  test("toSensorDataRow should convert string value to option of SensorDataRow") {
    assert(FileProcessor.toSensorDataRow("test").isEmpty)
    assertResult(Some(SensorDataRow("a", "1")))(FileProcessor.toSensorDataRow("a,1"))
  }

  test("combineMaps should merge maps properly") {
    val first = Map("s1" -> SensorStat(10, 10, 1, 10L, 1, 0), "s2" -> SensorStat(88, 88, 0, 88L, 1, 0))
    val second = Map("s2" -> SensorStat(78, 80, 1, 158L, 2, 0), "s3" -> SensorStat(1, 1, 1, 1L, 1, 0), "s1" -> SensorStat(98, 98, 0, 98L, 1, 0))
    val expectedResult = Map("s1" -> SensorStat(10, 98, 1, 108L, 2, 0), "s2" -> SensorStat(78, 88, 1, 246L, 3, 0), "s3" -> SensorStat(1, 1, 1, 1L, 1, 0))

    assertResult(expectedResult)(FileProcessor.combineMaps(first, second))
  }

  test("calculateStatistics should sort elements in descending order by average temperature") {
    val expectedResult = Seq(("s2", SensorStat(88, 88, 0, 88, 1, 88.0)), ("s1", SensorStat(10, 10, 1, 10, 1, 10.0)))
    val actualResult = FileProcessor.calculateStatistics(Array(absolutePath + "leader-1.csv"))
    assertResult(expectedResult)(actualResult)
  }


}
