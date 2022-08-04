object DataProcessor {

  def processData(map: Map[String, (Int, Int, Int, Long, Int)], item: (String, String)): Map[String, (Int, Int, Int, Long, Int)] = {
    val (min, max, nan, sum, count) = map.getOrElse(item._1, (Int.MaxValue, Int.MinValue, 0, 0L, 0))
    item._2 match {
      case "NaN" => map + (item._1 -> (min, max, nan + 1, sum, count))
      case _ if item._2.matches("\\d+") =>
        val temperature = item._2.toInt
        map + (item._1 -> (min.min(temperature), max.max(temperature), nan, sum + temperature, count + 1))
      case _ => map
    }
  }

  def addAvgToTuple(sensorStats: Map[String, (Int, Int, Int, Long, Int)]): Map[String, (Int, Int, Int, Long, Int, Double)] =
    sensorStats.map { case (k, v) => k -> (v._1, v._2, v._3, v._4, v._5, calculateAvg(v._4, v._5)) }

  def calculateAvg(sum: Long, count: Int): Double =
    count match {
      case 0 => Double.NaN
      case _ => sum.toDouble / count
    }

}
