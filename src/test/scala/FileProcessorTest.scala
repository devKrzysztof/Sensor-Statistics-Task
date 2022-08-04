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
    println(FileProcessor.fileProcessor(absolutePath + "leader-1.csv"))
    println(FileProcessor.fileProcessor(absolutePath + "leader-2.csv"))
    val expectedResult = Map("s1" -> (10, 10, 1, 10L, 1), "s2" -> (88, 88, 0, 88L, 1))

    assertResult(expectedResult)(processedFile)
  }

  test("toTuple should convert option of string value to two-values tuple") {
    assert(FileProcessor.toTuple("test").isEmpty)
    assertResult(Some("a","1"))(FileProcessor.toTuple("a,1"))
  }

  test("combineMaps should merge maps properly") {
    val first = Map("s1" -> (10, 10, 1, 10L, 1), "s2" -> (88, 88, 0, 88L, 1))
    val second = Map("s2" -> (78, 80, 1, 158L, 2), "s3" -> (1, 1, 1, 1L, 1), "s1" -> (98, 98, 0, 98L, 1))
    val expectedResult = Map("s1" -> (10, 98, 1, 108L, 2), "s2" -> (78, 88, 1, 246L, 3), "s3" -> (1, 1, 1, 1L, 1))

    assertResult(expectedResult)(FileProcessor.combineMaps(first, second))
  }

  test("calculateStatistics should sort elements in descending order by average temperature") {
    val expectedResult = Seq(("s2", (88, 88, 0, 88, 1, 88.0)), ("s1", (10, 10, 1, 10, 1, 10.0)))
    val actualResult = FileProcessor.calculateStatistics(Array(absolutePath + "leader-1.csv"))
    assertResult(expectedResult)(actualResult)
  }


}
