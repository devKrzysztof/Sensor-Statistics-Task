object StatsPrinter {

  def printStatistics(filesRead: Int, sensorStats: Seq[(String, SensorStat)]): Unit = {
    val (nan, count) = overallStats(sensorStats)
    println("Num of processed files: " + filesRead)
    println("Num of processed measurements: " + (nan + count))
    println("Num of failed measurements: " + nan)

    if (sensorStats.nonEmpty) {
      println("\nSensors with highest avg humidity:\n")
      println("sensor-id,min,avg,max")
    }
    sensorStats.foreach(sensorStatsPrinter)
  }

  private def sensorStatsPrinter(sensor: (String, SensorStat)): Unit = {
    def outputFormatter(sensorData: AnyVal): String = if (sensor._2.avg.isNaN) "NaN" else sensorData.toString

    val avg = outputFormatter(sensor._2.avg.toInt)
    val min = outputFormatter(sensor._2.min)
    val max = outputFormatter(sensor._2.max)

    println(sensor._1 + "," + min + "," + avg + "," + max)
  }

  def overallStats: Seq[(String, SensorStat)] => (Int, Int) =
    _.foldLeft((0, 0)) { (tpl, item) => (tpl._1 + item._2.nan, tpl._2 + item._2.count) }

}
