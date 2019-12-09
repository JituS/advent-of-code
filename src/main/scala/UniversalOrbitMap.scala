import scala.io.Source

object UniversalOrbitMap {

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }

  def main(args: Array[String]): Unit = {
    val map = readFileAsLines("universal_orbit_map.txt").map(_.split("\\)", 2) match {
      case Array(key, value) => value -> key
    }).toMap
    val value = map.map { each: (String, String) =>
      (each._1, map.foldLeft(Array(each._2)) { (acc, _) =>
        acc :+ map.getOrElse(acc.last, "")
      }.filter(!_.equals("")))
    }
    val common = value("YOU").intersect(value("SAN"))
    println(value("YOU").count(!common.contains(_)) + value("SAN").count(!common.contains(_)))
  }
}
