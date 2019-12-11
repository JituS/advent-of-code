//import scala.io.Source
//
//object MonitoringStation {
//  private def readFileAsLines(fileName: String): Seq[String] = {
//    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
//    (for (line <- bufferedSource.getLines()) yield line).toSeq
//  }
//
//  def main(args: Array[String]): Unit = {
//    val astroidMap = readFileAsLines("map.txt").map { e =>
//      e.split("")
//    }
//    val (maxCol, maxRow) =
//    astroidMap.zipWithIndex.map{ col =>
//      col._1.zipWithIndex.map{ row =>
//        val currentCol = col._2
//        val currentRow = row._2
//
//
//
//
//
//      }
//    }
//  }
//}
