import scala.annotation.tailrec
import scala.io.Source

object Computer {

  val PAUSED = "paused"
  val HALTED = "halted"
  val RUNNING = "running"

  def getIndex(opcode: String): Int = {
    opcode match {
      case a if List("4", "3").exists(a.endsWith) => 1
      case a if List("5", "6").exists(a.endsWith) => 2
      case _ => 3
    }
  }

  case class Amplifier(calculatedValue: Option[String], outputIndex: Int, generatedOutput: Iterator[Int], state: String)

  private def handleParameterMode(i: Int, inputArray: Array[String], inputs: Iterator[Int]) = {
    println(i)
    println(inputArray.mkString(","))
    val operation = inputArray(i).split("").reverse.padTo(5, "0").reverse
    val operand = operation.takeRight(2).mkString
    operation.dropRight(2) match {
      case Array(a, b, c) =>
        operand match {
          case "01" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            Amplifier(Some((value1.toInt + value2.toInt).toString), i + getIndex(operand), inputs, RUNNING)
          case "02" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            Amplifier(Some((value1.toInt * value2.toInt).toString), i + getIndex(operand), inputs, RUNNING)
          case "03" =>
            Amplifier(Some(inputs.next().toString), i + getIndex(operand), inputs, RUNNING)
          case "04" =>
            val value1 = if (c == "0") inputArray(inputArray(i + 1).toInt) else inputArray(i + 1)
            Amplifier(None, i + getIndex(operand), inputs ++ Iterator(value1.toInt), RUNNING)
          case "05" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            Amplifier(None, if (value1.toInt != 0) value2.toInt - 1 else i + getIndex(operand), inputs, RUNNING)
          case "06" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            Amplifier(None, if (value1.toInt == 0) value2.toInt - 1 else i + getIndex(operand), inputs, RUNNING)
          case "07" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            Amplifier(Some((if (value1 < value2) 1 else 0).toString), i + getIndex(operand), inputs, RUNNING)
          case "08" =>
            val (value1, value2) = getValues(i, inputArray, b, c)
            Amplifier(Some((if (value1 == value2) 1 else 0).toString), i + getIndex(operand), inputs, RUNNING)
          case "99" =>
            println(i, "halted")
            println(inputArray.mkString(","))
            InstructionResult(None, 0, inputs, state = true)
        }
    }
  }

  private def getValues(i: Int, inputArray: Array[String], b: String, c: String) = {
    val value1 = if (c == "0") inputArray(inputArray(i + 1).toInt) else inputArray(i + 1)
    val value2 = if (b == "0") inputArray(inputArray(i + 2).toInt) else inputArray(i + 2)
    (value1.toInt, value2.toInt)
  }

  private def readFileAsLines() = {
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream("computer_input.txt"))
    for (line <- bufferedSource.getLines()) yield line
  }

  @tailrec def processInput(currentIndex: Int, inputArray: Array[String], inputs: Iterator[Int]): Amplifier = {
    val instructionResult = handleParameterMode(currentIndex, inputArray, inputs)
    if (instructionResult.state) return instructionResult
    instructionResult.calculatedValue.foreach(inputArray(inputArray(instructionResult.outputIndex).toInt) = _)
    processInput(instructionResult.outputIndex + 1, inputArray, inputs ++ instructionResult.generatedOutput)
  }

  def main(args: Array[String]): Unit = {
    val input = readFileAsLines().next().split(",")
    //    println((0 to 4).permutations.map(feedbackLoop(_, input)._1).max)
    feedbackLoop(Array(9, 8, 7, 6, 5), input)._1.foreach(println)
  }

  def feedbackLoop(signal: Seq[Int], input: Array[String], finalOutput: (Iterator[Int], Boolean) = (Iterator(0), false)): (Iterator[Int], Boolean) = {
    //    if (false) return finalOutput
    signal.map((_, input)).foldLeft(0){ (initialInput: Int, amplifier: (Int, Array[String])) =>
      processInput()
    }

//    signal.map().foldLeft(finalOutput) { (a, b) =>
//      val output: InstructionResult = processInput(0, input, Iterator(b) ++ a._1)
//      val c = output.generatedOutput.toList
//      (c.toIterator, false)
      //      println(output)
      //      if (output.isHalted) return (output.generatedOutput, false)
      //      else (output.generatedOutput, false)
//    }
    //    println("signal, input.mkString(\",\"), output -> " + signal, input.mkString(","), output)
    //    feedbackLoop(signal, input, output)
  }
}