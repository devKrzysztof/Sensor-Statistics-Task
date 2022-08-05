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

  def calculateStatistics: Array[String] => Seq[(String, SensorStat)] =
    _
      .map(fileProcessor)
      .reduceOption(combineMaps)
      .map(DataProcessor.addAvgToTuple)
      .getOrElse(Map())
      .toSeq
      .sortBy { case (_, SensorStat(_, _, _, _, _, avg)) => -avg }

  def getFilesArray(path: String): Option[Array[File]] =
    Option(new File(path).listFiles)

  def getCSVsInPath: Array[File] => Array[String] =
    _
      .filter(_.isFile)
      .filter(_.canRead)
      .filter(_.getName.endsWith(".csv"))
      .map(_.getAbsolutePath)

  def fileProcessor(file: String): Map[String, SensorStat] = {
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

  def processFile: BufferedSource => Map[String, SensorStat] =
    _
      .getLines
      .drop(1)
      .map(toTuple)
      .filter(_.isDefined)
      .map(_.get)
      .foldLeft(Map[String, SensorStat]())((map, item) => DataProcessor.processData(map, item))

  def toTuple: String => Option[(String, String)] = _.split(',') match {
    case Array(sensor, temperature) => Some(sensor, temperature)
    case _ => None
  }

  def combineMaps(x: Map[String, SensorStat],
                  y: Map[String, SensorStat]): Map[String, SensorStat] =
    x ++ y.map { case (k, v) => k -> combineValues(v, x.get(k)) }

  private def combineValues(x: SensorStat, y: Option[SensorStat]): SensorStat =
    y match {
      case Some(value) => SensorStat(x.min.min(value.min), x.max.max(value.max), x.nan + value.nan, x.sum + value.sum, x.count + value.count, x.avg)
      case None => x
    }

}
