import scala.annotation.tailrec
import scala.io.Source

object Computer {
  def main(args: Array[String]): Unit = {
    @tailrec def operation(i: Int, array: Array[Int]) {
      if (i < array.length) {
        val opCode = array(i)
        if (opCode == 99) return
        val operand1 = array(i + 1)
        val operand2 = array(i + 2)
        array(array(i + 3)) = opCode match {
          case 1 => array(operand1) + array(operand2)
          case 2 => array(operand1) * array(operand2)
        }
        operation(i + 4, array)
      }
    }

    val input = readFileAsLines("computer_input.txt").next().split(",").map(_.toInt)
    input(1) = 12
    input(2) = 2
    operation(0, input)
    println(input(0))
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }
}
