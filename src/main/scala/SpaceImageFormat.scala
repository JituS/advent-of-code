import scala.io.Source

object SpaceImageFormat {
  implicit def ordered: Ordering[Array[Array[String]]] = new Ordering[Array[Array[String]]] {
    def compare(x: Array[Array[String]], y: Array[Array[String]]): Int = {
      x.foldLeft(0)((acc, array) => acc + array.count(_ == "0")) compareTo y.foldLeft(0)((acc, array) => acc + array.count(_ == "0"))
    }
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }

  def main(args: Array[String]): Unit = {
    val input = readFileAsLines("image.txt").next().split("")
    val width = 25
    val height = 6
    val output = input.grouped(width * height).toArray.map(_.grouped(width).toArray)
    output.reduce { (a, b) => {
      (a zip b).map { case (aList, bList) =>
        (aList zip bList).map { case (aItem, bItem) =>
          if (aItem == "2") bItem else aItem
        }
      }
    }
    }.foreach(e => {
      e.foreach(a => if (a != "0") print(a) else print(" "))
      println()
    })
  }
}