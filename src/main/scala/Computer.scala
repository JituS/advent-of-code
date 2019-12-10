import scala.annotation.tailrec
import scala.io.Source

object Computer {

  def getIndex(opcode: Int): Int = {
    opcode match {
      case 3 => 1
      case 4 => 1
      case 104 => 1
      case _ => 3
    }
  }

  def main(args: Array[String]): Unit = {
    @tailrec def operation(i: Int, array: Array[Int]) {
      if (array(i) == 4) {
        println(array(array(i + 1)))
      }else if(array(i) == 104){
        println(array(i + 1))
      }
      else {
        val i1 = array(i + getIndex(array(i)))
        array(i1) = array(i) match {
          case 1 => array(array(i + 1)) + array(array(i + 2))
          case 2 => array(array(i + 1)) * array(array(i + 2))
          case 3 => readInt
          case _ =>
            val operationToPerform = array(i).toString.takeRight(2).toInt
            val modes = array(i).toString.dropRight(2).split("").reverse.padTo(3, "0").reverse
            modes match {
              case Array(a, b, c) =>
                val value1 = if (c == "0") array(array(i + 1)) else array(i + 1)
                val value2 = if (b == "0") array(array(i + 2)) else array(i + 2)
                println()
                operationToPerform match {
                  case 1 => value1 + value2
                  case 2 => value1 * value2
                }
            }
        }
      }
      operation(i + getIndex(array(i)) + 1, array)
    }

    val input = readFileAsLines("computer_input.txt").next().split(",").map(_.toInt)
    operation(0, input)
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }
}
