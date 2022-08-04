object StatsPrinter {

  def printStatistics(filesRead: Int, sensorStats: Seq[(String, (Int, Int, Int, Long, Int, Double))]): Unit = {
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

  private def sensorStatsPrinter(sensor: (String, (Int, Int, Int, Long, Int, Double))): Unit = {
    def outputFormatter(sensorData: AnyVal): String = if (sensor._2._6.isNaN) "NaN" else sensorData.toString

    val avg = outputFormatter(sensor._2._6.toInt)
    val min = outputFormatter(sensor._2._1)
    val max = outputFormatter(sensor._2._2)

    println(sensor._1 + "," + min + "," + avg + "," + max)
  }

  def overallStats: Seq[(String, (Int, Int, Int, Long, Int, Double))] => (Int, Int) =
    _.foldLeft((0, 0)) { (tpl, item) => (tpl._1 + item._2._3, tpl._2 + item._2._5) }

}
