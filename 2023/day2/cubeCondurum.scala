//> using toolkit latest
//> using lib com.lihaoyi::fastparse:3.0.2

import fastparse.*, SingleLineWhitespace.*

enum Color:
  case red
  case green
  case blue

case class Game(id: Int, cubes: Seq[(Int, Color)])

def parser[$: P] =
  def num = CharIn("0-9").rep.!.map(_.toInt)
  def color = ("red" | "blue" | "green").!.map(Color.valueOf)
  def turn = (num ~ color).rep(sep = ",")
  P("Game" ~ num ~ ":" ~ turn.rep(sep = ";") ~ End)

def parseGames(lines: String) =
  lines
    .split("\n")
    .map: line =>
      val (id, moves) = parse(line, parser).get.value
      Game(id, moves.flatten)
    .toList

def answer1(games: List[Game]) =
  val limits = Map(
    Color.red -> 12,
    Color.green -> 13,
    Color.blue -> 14
  )
  val correct = games.filter: game =>
    game.cubes
      .groupMapReduce(_._2)(_._1)(_ max _)
      .forall: (c, l) =>
        limits(c) >= l
  correct.map(_.id).sum

def answer2(games: List[Game]) =
  val powers = games.map: game =>
    game.cubes
      .groupMapReduce(_._2)(_._1)(_ max _)
      .values
      .reduce(_ * _)
  powers.sum

@main def main =
  val input = os.read(os.pwd / "input1.txt")
  val games = parseGames(input)
  println(answer1(games))
  println(answer2(games))
