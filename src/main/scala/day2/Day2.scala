package day2

import scala.io.Source

def input: Iterator[String] = Source.fromResource("day2.txt").getLines

def part1: Int =
  var x = 0
  var y = 0
  for (dx, dy) <- input.map(lineToDelta) do
    x += dx
    y += dy
  x * y

// Returns the pair of (change to horizontal position, change to depth or aim) for a given line
// Depth for part 1, aim for part 2
def lineToDelta(line: String): (Int, Int) =
  val parts = line.split(" ").nn
  val cmd = parts(0).nn
  val amount = parts(1).nn.toInt
  cmd match
    case "forward" => (amount, 0)
    case "up"      => (0, -amount)
    case "down"    => (0, amount)

def part2: Int =
  var x = 0
  var y = 0
  var aim = 0
  for (dx, daim) <- input.map(lineToDelta) do
    x += dx
    aim += daim
    y += aim * dx
  x * y

def part2WithFold: Int =
  val (xFinal, yFinal, _) = input
    .map(lineToDelta)
    .foldLeft((0, 0, 0))((acc, value) =>
      (acc, value) match
        case ((x, y, aim), (dx, daim)) =>
          (x + dx, y + (aim + daim) * dx, aim + daim)
    )
  xFinal * yFinal
