import scala.io.Source

object FuelCalculator {
  def main(args: Array[String]): Unit = {
    print(readFileAsLines("mass.txt").foldLeft[Int](0)((a, b) => calculate_fuel_from_mass(b) + a))
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    (for (line <- bufferedSource.getLines()) yield line).toSeq
  }

  def calculate_fuel_from_mass(mass: String): Int = {
    ((mass.toInt / 3).floor - 2.0).toInt
  }
}
