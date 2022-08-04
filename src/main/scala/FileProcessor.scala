import java.io.{File, FileNotFoundException}
import scala.io.{BufferedSource, Source}

object FileProcessor {

  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) {
      val path = args(0)
      processFiles(path)
    } else {
      println("No path to CSV files provided")
    }
  }

  private def processFiles(path: String): Unit = {
    getFilesArray(path) match {
      case Some(filePaths) =>
        val csvFiles = getCSVsInPath(filePaths)
        StatsPrinter.printStatistics(csvFiles.length, calculateStatistics(csvFiles))
      case None => println("There was a problem with accessing given path: " + path)
    }
  }

  def calculateStatistics: Array[String] => Seq[(String, (Int, Int, Int, Long, Int, Double))] =
    _
      .map(fileProcessor)
      .reduceOption(combineMaps)
      .map(DataProcessor.addAvgToTuple)
      .getOrElse(Map())
      .toSeq
      .sortBy { case (_, (_, _, _, _, _, avg)) => -avg }

  def getFilesArray(path: String): Option[Array[File]] =
    Option(new File(path).listFiles)

  def getCSVsInPath: Array[File] => Array[String] =
    _
      .filter(_.isFile)
      .filter(_.canRead)
      .filter(_.getName.endsWith(".csv"))
      .map(_.getAbsolutePath)

  def fileProcessor(file: String): Map[String, (Int, Int, Int, Long, Int)] = {
    try {
      val source = Source.fromFile(file)
      try {
        processFile(source)
      } finally source.close()
    } catch {
      case _: FileNotFoundException =>
        println(s"File $file cannot be accessed")
        Map()
    }
  }

  def processFile: BufferedSource => Map[String, (Int, Int, Int, Long, Int)] =
    _
      .getLines
      .drop(1)
      .map(toTuple)
      .filter(_.isDefined)
      .map(_.get)
      .foldLeft(Map[String, (Int, Int, Int, Long, Int)]())((map, item) => DataProcessor.processData(map, item))

  def toTuple: String => Option[(String, String)] = _.split(',') match {
    case Array(sensor, temperature) => Some(sensor, temperature)
    case _ => None
  }

  def combineMaps(x: Map[String, (Int, Int, Int, Long, Int)],
                  y: Map[String, (Int, Int, Int, Long, Int)]): Map[String, (Int, Int, Int, Long, Int)] =
    x ++ y.map { case (k, v) => k -> combineValues(v, x.get(k)) }

  private def combineValues(x: (Int, Int, Int, Long, Int),
                            y: Option[(Int, Int, Int, Long, Int)]): (Int, Int, Int, Long, Int) =
    y match {
      case Some(value) => (x._1.min(value._1), x._2.max(value._2), x._3 + value._3, x._4 + value._4, x._5 + value._5)
      case None => x
    }

}
