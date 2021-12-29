package day25

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

enum Square:
  override def toString: String = this match
    case Empty => "."
    case South => "v"
    case East  => ">"

  case Empty, South, East

object Square:
  def apply(s: Char): Square = s match
    case '.' => Empty
    case '>' => East
    case 'v' => South
    case _   => ???

class CucumberGrid(val grid: ArrayBuffer[ArrayBuffer[Square]]):
  def width: Int = grid(0).size
  def height: Int = grid.size

  override def toString: String = grid.map(_.mkString).mkString("\n")

  // return true if any cucumber moved
  def step(): Boolean =
    // first pass: pull the coordinates of all east the WILL move, and the coordinates of all south
    // then execute the east movement
    // then find all south that will move
    // then execute the south movement
    var eastWillMove = List[(Int, Int)]()
    var southSquares = List[(Int, Int)]()
    for
      row <- 0 until height
      col <- 0 until width
    do
      grid(row)(col) match
        case Square.South => southSquares = (row, col) :: southSquares
        case Square.East if grid(row)((col + 1) % width) == Square.Empty =>
          eastWillMove = (row, col) :: eastWillMove
        case _ => {}
    for (row, col) <- eastWillMove do
      grid(row)(col) = Square.Empty
      grid(row)((col + 1) % width) = Square.East

    var anyMoved = !eastWillMove.isEmpty
    var southWillMove = List[(Int, Int)]()
    for (row, col) <- southSquares do
      if grid((row + 1) % height)(col) == Square.Empty then
        southWillMove = (row, col) :: southWillMove
    for (row, col) <- southWillMove do
      grid(row)(col) = Square.Empty
      grid((row + 1) % height)(col) = Square.South

    anyMoved ||= !southWillMove.isEmpty
    anyMoved

  def stepUntilStopped(): Int =
    var counter = 1
    while step() do counter += 1
    counter

object CucumberGrid:
  def fromString(source: String): CucumberGrid =
    val buf = ArrayBuffer[ArrayBuffer[Square]]()
    for line <- source.linesIterator do
      buf += ArrayBuffer.from(line.map(Square.apply))
    CucumberGrid(buf)

def exampleGrid(): CucumberGrid = CucumberGrid.fromString("""..........
.>v....v..
.......>..
..........
""")

def exampleGrid2(): CucumberGrid = CucumberGrid.fromString("""...>...
.......
......>
v.....>
......>
.......
..vvv..
""")

def exampleGrid3(): CucumberGrid = CucumberGrid.fromString("""v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
""")

def inputGrid(): CucumberGrid =
  val s = Source.fromResource("day25.txt").mkString
  CucumberGrid.fromString(s)

def part1(): Int =
  val grid = inputGrid()
  val ret = grid.stepUntilStopped()
  println(grid)
  ret
