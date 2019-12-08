import scala.annotation.tailrec
import scala.io.Source

object Computer {
  def main(args: Array[String]): Unit = {
    @tailrec def operation(instructionPointer: Int, inputValues: Array[Int]) {
      if (instructionPointer < inputValues.length) {
        val opCode = inputValues(instructionPointer)
        if (opCode == 99) return
        val operand1 = inputValues(instructionPointer + 1)
        val operand2 = inputValues(instructionPointer + 2)
        inputValues(inputValues(instructionPointer + 3)) = opCode match {
          case 1 => inputValues(operand1) + inputValues(operand2)
          case 2 => inputValues(operand1) * inputValues(operand2)
        }
        operation(instructionPointer + 4, inputValues)
      }
    }

    val input = readFileAsLines("computer_input.txt").next().split(",").map(_.toInt)
    val operableInput = input.clone
    var outerLoopCounter = 0
    var innerLoopCounter = 0
    while (outerLoopCounter <= 99 && operableInput(0) != 19690720) {
      innerLoopCounter = 0
      while (innerLoopCounter <= 99 && operableInput(0) != 19690720) {
        input.copyToArray(operableInput)
        operableInput(1) = outerLoopCounter
        operableInput(2) = innerLoopCounter
        operation(0, operableInput)
        innerLoopCounter = innerLoopCounter + 1
      }
      outerLoopCounter = outerLoopCounter + 1
    }
    println(innerLoopCounter-1, outerLoopCounter-1)
  }

  private def readFileAsLines(fileName: String) = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))
    for (line <- bufferedSource.getLines()) yield line
  }
}
