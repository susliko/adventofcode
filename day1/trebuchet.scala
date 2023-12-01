//> using toolkit latest

def answer1(input: String) =
  input
    .split("\n")
    .flatMap(line =>
      for
        char1 <- line.find(_.isDigit)
        char2 <- line.findLast(_.isDigit)
        twoDigit = char1.asDigit * 10 + char2.asDigit.toInt
      yield twoDigit
    )
    .sum

enum Digit(val value: Int):
  case zero extends Digit(0)
  case one extends Digit(1)
  case two extends Digit(2)
  case three extends Digit(3)
  case four extends Digit(4)
  case five extends Digit(5)
  case six extends Digit(6)
  case seven extends Digit(7)
  case eight extends Digit(8)
  case nine extends Digit(9)

def findInLine(line: String) =
  val data = Digit.values ++ (0 to 10)
  def find(f: String => String => Int) =
    data
      .flatMap: x =>
        val ind = f(line)(x.toString)
        val res = x match
          case i: Int   => i
          case d: Digit => d.value
        Option.when(ind >= 0)((ind, res))

  val first = find(_.indexOf).minBy(_._1)._2
  val second = find(_.lastIndexOf).maxBy(_._1)._2
  first * 10 + second

def answer2(input: String) =
  input.split("\n").toList.map(findInLine)

@main def main =
  // val input1 = os.read(os.pwd / "input1.txt")
  // println(answer1(input1))
  val input2 = os.read(os.pwd / "input2.txt")
  println(answer2(input2).sum)
