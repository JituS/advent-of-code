object SecureContainer {
  def validate(number: String): Boolean = {
    number.sorted.equals(number) && number.foldLeft(Array(Array[Char]()))((a, b) => {
      if (a.last.contains(b)) a.dropRight(1) :+ (a.last :+ b)
      else a :+ Array(b)
    }).count(e => e.length == 2) >= 1
  }

  def main(args: Array[String]): Unit = {
    println((356261 to 846303).count(e => validate(e.toString)))
  }
}
