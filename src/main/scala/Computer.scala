import scala.annotation.tailrec
import scala.io.{Source, StdIn}

object Computer {

  def getIndex(opcode: String): Int = {
    opcode match {
      case a if a.endsWith("4") || a.endsWith("3") => 1
      case _ => 3
    }
  }

  private def handleParameterMode(i: Int, inputArray: Array[String]) = {
    val operation = inputArray(i).split("").reverse.padTo(5, "0").reverse
    val operand = operation.takeRight(2).mkString
    val modes: Array[String] = operation.dropRight(2)
    modes match {
      case Array(a, b, c) =>
        val value1 = if (c == "0") inputArray(inputArray(i + 1).toInt) else inputArray(i + 1)
        val value2 = if (b == "0" && !operand.endsWith("4")) inputArray(inputArray(i + 2).toInt) else inputArray(i + 2)
        calculate(operand, value1, value2)
    }
  }

  def calculate(operation: String, val1: String, val2: String): String = {
    (operation match {
      case "01" => val1.toInt + val2.toInt
      case "02" => val1.toInt * val2.toInt
      case "03" => StdIn.readLine
      case "04" =>
        println(val1)
        val1
    }).toString
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }

  @tailrec def processInput(currentIndex: Int, inputArray: Array[String]) {
    val outputIndex = inputArray(currentIndex + getIndex(inputArray(currentIndex))).toInt
    inputArray(outputIndex) = handleParameterMode(currentIndex, inputArray)
    processInput(currentIndex + getIndex(inputArray(currentIndex)) + 1, inputArray)
  }

  def main(args: Array[String]): Unit = {
    val input = readFileAsLines("computer_input.txt").next().split(",")
    processInput(0, input)
  }
}