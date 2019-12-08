import scala.io.Source

object FuelCalculator {
  def main(args: Array[String]): Unit = {
    println(readFileAsLines("mass.txt").foldLeft[Int](0)((a, b) => calculateFuelFromMass(b) + a))
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    (for (line <- bufferedSource.getLines()) yield line.toInt).toSeq
  }

  def calculateFuelFromMass(mass: Int): Int = {
    val fuelMass = ((mass / 3).floor - 2.0).toInt
    if (fuelMass <= 0) return 0
    fuelMass + calculateFuelFromMass(fuelMass)
  }
}
