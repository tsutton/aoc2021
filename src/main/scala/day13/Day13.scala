package day13

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

def input: Iterator[String] =
  Source.fromResource("day13.txt").getLines

def exampleInput: Iterator[String] = """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
""".linesIterator

enum Axis:
  case X
  case Y

def parse(lines: Iterator[String]): (Grid, List[(Axis, Int)]) =
  val grid = Grid()
  for line <- lines.takeWhile(_ != "") do
    val parts = line.split(',')
    grid.addPoint(parts(0).toInt, parts(1).toInt)

  val folds = lines
    .map(line => line.substring(11).nn.split('='))
    .map(l =>
      l(0) match
        case "x" => (Axis.X, l(1).toInt)
        case "y" => (Axis.Y, l(1).toInt)
    )
    .toList
  (grid, folds)

def part1(): Int =
  val (grid, folds) = parse(input)
  // println(grid)
  // folds.foreach((a, i) => println(s"fold ${a}=${i}"))
  val newGrid = grid.fold(folds(0)(0), folds(0)(1))
  newGrid.size

def part2(): Int =
  var (grid, folds) = parse(input)
  for (axis, value) <- folds do grid = grid.fold(axis, value)
  println(grid.prettyGrid())
  0

class Grid():
  val grid: HashSet[(Int, Int)] = HashSet()
  def addPoint(pt: (Int, Int)) = grid += pt

  def fold(axis: Axis, value: Int): Grid = axis match
    case Axis.X => foldX(value)
    case Axis.Y => foldY(value)

  def foldX(axis: Int): Grid =
    val newGrid = Grid()
    for (x, y) <- grid do
      if x < axis then newGrid.addPoint(x, y)
      else newGrid.addPoint(2 * axis - x, y)
    newGrid

  def foldY(axis: Int): Grid =
    val newGrid = Grid()
    for (x, y) <- grid do
      if y < axis then newGrid.addPoint(x, y)
      else newGrid.addPoint(x, 2 * axis - y)
    newGrid

  def size: Int = grid.size

  override def toString(): String = grid.mkString("\n")

  def prettyGrid(): String =
    val maxX: Int = grid.maxBy(_(0)).apply(0) + 1
    val maxY: Int = grid.maxBy(_(1)).apply(1) + 1
    val s: ArrayBuffer[ArrayBuffer[Char]] =
      ArrayBuffer.fill(maxY, maxX)('.')
    for (x, y) <- grid do s(y)(x) = '#'
    s.map(_.mkString("")).mkString("\n")
