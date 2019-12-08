import scala.io.Source

//
object CrossedWires {
  //  def main(args: Array[String]): Unit = {
  //    val lines = readFileAsLines("wire_paths.txt")
  //    val matrix = Array.ofDim[String](25000, 25000)
  //    val intersectionPoints: ListBuffer[(Int, Int)] = new ListBuffer[(Int, Int)]()
  //    drawWire(lines.head, matrix, "X", Array())
  //    drawWire(lines(1), matrix, "Y", Array("X"))
  //    for (i: (Array[String], Int) <- matrix.zipWithIndex) {
  //      for (j: (String, Int) <- matrix(i._2).zipWithIndex) {
  //        if (i._2 != 12000 && j._2 != 12000 && matrix(i._2)(j._2) == "O") intersectionPoints.append((i._2, j._2))
  //      }
  //    }
  //    println(intersectionPoints.foldLeft[Double](PositiveInfinity)((a, b) => if (math.abs(b._1 - 12000) + math.abs(b._2 - 12000) < a) math.abs(b._1 - 12000) + math.abs(b._2 - 12000) else a))
  //  }
  //
  //  private def drawWire(line: String, matrix: Array[Array[String]], symbol: String, existingWireSymbol: Array[String]) = {
  //    var centrePoint = (12000, 12000)
  //    line.split(",").foreach { move =>
  //      val direction = move.substring(0, 1)
  //      val steps = move.substring(1).toInt
  //      direction match {
  //        case "R" =>
  //          for (i <- centrePoint._2 to centrePoint._2 + steps) {
  //            if (existingWireSymbol.contains(matrix(centrePoint._1)(i))) matrix(centrePoint._1)(i) = "O"
  //            else matrix(centrePoint._1)(i) = symbol
  //          }
  //          centrePoint = (centrePoint._1, centrePoint._2 + steps)
  //        case "L" =>
  //          for (i <- centrePoint._2 - steps to centrePoint._2) {
  //            if (existingWireSymbol.contains(matrix(centrePoint._1)(i))) matrix(centrePoint._1)(i) = "O"
  //            else matrix(centrePoint._1)(i) = symbol
  //          }
  //          centrePoint = (centrePoint._1, centrePoint._2 - steps)
  //        case "U" =>
  //          for (i <- centrePoint._1 to centrePoint._1 + steps) {
  //            if (existingWireSymbol.contains(matrix(i)(centrePoint._2))) matrix(i)(centrePoint._2) = "O"
  //            else matrix(i)(centrePoint._2) = symbol
  //          }
  //          centrePoint = (centrePoint._1 + steps, centrePoint._2)
  //        case "D" =>
  //          for (i <- centrePoint._1 - steps to centrePoint._1) {
  //            if (existingWireSymbol.contains(matrix(i)(centrePoint._2))) matrix(i)(centrePoint._2) = "O"
  //            else matrix(i)(centrePoint._2) = symbol
  //          }
  //          centrePoint = (centrePoint._1 - steps, centrePoint._2)
  //      }
  //    }
  //    matrix
  //  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    (for (line <- bufferedSource.getLines()) yield line).toSeq
  }

  def lineIntersection(line1: Array[(Int, Int)], line2: Array[(Int, Int)]): Option[(Int, Int)] = {
    if (line1(0)._2 == line1(1)._2) {
      val sortedX = Array(line1(0)._1, line1(1)._1).sorted
      val sortedY = Array(line2(0)._2, line2(1)._2).sorted
      if ((sortedX(0) to sortedX(1) contains line2(0)._1) && (sortedY(0) to sortedY(1) contains line1(0)._2)) Some((line2(0)._1, line1(0)._2))
      else None
    } else {
      val sortedX = Array(line2(0)._1, line2(1)._1).sorted
      val sortedY = Array(line1(0)._2, line1(1)._2).sorted
      if ((sortedX(0) to sortedX(1) contains line1(0)._1) && (sortedY(0) to sortedY(1) contains line2(0)._2)) Some((line1(0)._1, line2(1)._2))
      else None
    }
  }

  def main(args: Array[String]): Unit = {
    val cords = readFileAsLines("wire_paths.txt")
      .map { line =>
        val moves = line.split(",")
        var currentCords = (0, 0)
        moves.map { move =>
          val direction = move.substring(0, 1)
          val steps = move.substring(1).toInt
          direction match {
            case "R" => currentCords = (currentCords._1 + steps, currentCords._2)
              currentCords
            case "L" => currentCords = (currentCords._1 - steps, currentCords._2)
              currentCords
            case "U" => currentCords = (currentCords._1, currentCords._2 + steps)
              currentCords
            case "D" => currentCords = (currentCords._1, currentCords._2 - steps)
              currentCords
          }
        }
      }
    val firstWire = cords.head.sliding(2).toArray
    val secondWire = cords(1).sliding(2).toArray
    println(firstWire.flatMap { line1 =>
      secondWire.map { line2 =>
        val maybeTuple = lineIntersection(line1, line2)
        if(maybeTuple.isDefined && maybeTuple.get.equals((0, 0))) None else maybeTuple
      }
    }.filter(_.isDefined)
      .foldLeft(Double.PositiveInfinity)((a, b) => if (math.abs(b.get._1) + math.abs(b.get._2) <= a) math.abs(b.get._1) + math.abs(b.get._2) else a))
  }
}



