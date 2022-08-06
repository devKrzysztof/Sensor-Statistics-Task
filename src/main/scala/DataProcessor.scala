object DataProcessor {

  def processData(map: Map[String, SensorStat], item: SensorDataRow): Map[String, SensorStat] = {
    val stats = map.getOrElse(item.sensor, SensorStat(Int.MaxValue, Int.MinValue, 0, 0L, 0, 0))
    item.temperature match {
      case "NaN" => map + (item.sensor -> SensorStat(stats.min, stats.max, stats.nan + 1, stats.sum, stats.count, stats.avg))
      case _ if item.temperature.matches("\\d+") =>
        val temperature = item.temperature.toInt
        map + (item.sensor -> SensorStat(stats.min.min(temperature), stats.max.max(temperature), stats.nan, stats.sum + temperature, stats.count + 1, stats.avg))
      case _ => map
    }
  }

  def calculateAvgInSensorStat(sensorStats: Map[String, SensorStat]): Map[String, SensorStat] =
    sensorStats.map { case (k, v) => k -> SensorStat(v.min, v.max, v.nan, v.sum, v.count, calculateAvg(v.sum, v.count)) }

  def calculateAvg(sum: Long, count: Int): Double =
    count match {
      case 0 => Double.NaN
      case _ => sum.toDouble / count
    }

}
