import scala.io.Source

object UniversalOrbitMap {

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }

  def main(args: Array[String]): Unit = {
    val orbits = readFileAsLines("universal_orbit_map.txt").map(_.split("\\)", 2) match {
      case Array(key, value) => value -> key
    }).toMap
    println(orbits.map { orbit: (String, String) =>
      (orbit._1, orbits.foldLeft(Array(orbit._2)) { (acc, _) =>
        acc :+ orbits.getOrElse(acc.last, "")
      }.count(!_.equals("")))
    }.values.sum)
  }
}
