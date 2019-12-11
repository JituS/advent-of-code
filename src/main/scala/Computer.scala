import scala.annotation.tailrec
import scala.io.Source

object Computer {

  def getIndex(opcode: String): Int = {
    opcode match {
      case a if List("4", "3").exists(a.endsWith) => 1
      case a if List("5", "6").exists(a.endsWith) => 2
      case _ => 3
    }
  }

  private def handleParameterMode(i: Int, inputArray: Array[String], inputs: Iterator[Int]) = {
    val operation = inputArray(i).split("").reverse.padTo(5, "0").reverse
    val operand = operation.takeRight(2).mkString
    val modes: Array[String] = operation.dropRight(2)
    modes match {
      case Array(a, b, c) =>
        operand match {
          case "01" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            (Some((value1.toInt + value2.toInt).toString), i + getIndex(operand), "")
          case "02" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            (Some((value1.toInt * value2.toInt).toString), i + getIndex(operand), "")
          case "03" =>
            (Some(inputs.next().toString), i + getIndex(operand), "")
          case "04" =>
            val value1 = if (c == "0") inputArray(inputArray(i + 1).toInt) else inputArray(i + 1)
            (None, i + getIndex(operand), value1)
          case "05" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            (None, if (value1.toInt != 0) value2.toInt - 1 else i + getIndex(operand), "")
          case "06" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            (None, if (value1.toInt == 0) value2.toInt - 1 else i + getIndex(operand), "")
          case "07" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            (Some((if (value1 < value2) 1 else 0).toString), i + getIndex(operand), "")
          case "08" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            (Some((if (value1 == value2) 1 else 0).toString), i + getIndex(operand), "")
        }
    }
  }

  private def getValues(i: Int, inputArray: Array[String], b: String, c: String) = {
    val value1 = if (c == "0") inputArray(inputArray(i + 1).toInt) else inputArray(i + 1)
    val value2 = if (b == "0") inputArray(inputArray(i + 2).toInt) else inputArray(i + 2)
    (value1.toInt, value2.toInt)
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }

  @tailrec def processInput(currentIndex: Int, inputArray: Array[String], inputs: Iterator[Int]): String = {
    val tuple = handleParameterMode(currentIndex, inputArray, inputs)
    if (!tuple._3.isEmpty) return tuple._3
    tuple._1.foreach(inputArray(inputArray(tuple._2).toInt) = _)
    processInput(tuple._2 + 1, inputArray, inputs)
  }

  def main(args: Array[String]): Unit = {
    val input = readFileAsLines("computer_input.txt").next().split(",")
    println((0 to 4).permutations.map { e =>
      e.foldLeft(0) { (a, b) =>
        processInput(0, input, Iterator(b, a)).toInt
      }
    }.max)
  }
}