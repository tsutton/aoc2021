package day5

import scala.io.Source
import scala.collection.mutable.HashMap

def input: Iterator[String] = Source.fromResource("day5.txt").getLines
def exampleInput: Iterator[String] = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
""".linesIterator

def parse(): Iterator[Line] = input.map(Line.fromString)

def part1(): Int =
  val grid = Grid()
  val lines = parse()
  for
    line <- lines
    if line.is_horizontal_or_vertical
    point <- line.points
  do
    // println(s"${line}: ${point}")
    grid.addPoint(point._1, point._2)
  // println(grid.pointsWithOverlap.toList)
  grid.pointsWithOverlap.size

def part2(): Int =
  val grid = Grid()
  val lines = parse()
  for
    line <- lines
    point <- line.points
  do
    // println(s"${line}: ${point}")
    grid.addPoint(point._1, point._2)
  // println(grid.pointsWithOverlap.toList)
  grid.pointsWithOverlap.size

case class Line(val x1: Int, val y1: Int, val x2: Int, val y2: Int):
  def is_horizontal_or_vertical: Boolean = x1 == x2 || y1 == y2
  def points: Iterator[(Int, Int)] =
    val xStep = x2.compare(x1)
    val yStep = y2.compare(y1)
    if xStep == 0 then
      LazyList.range(y1, y2 + yStep, yStep).map((x1, _)).toIterator
    else if yStep == 0 then
      LazyList.range(x1, x2 + xStep, xStep).map((_, y1)).toIterator
    else
      LazyList
        .range(0, (x2 - x1).abs + 1)
        .map((i: Int) => (x1 + i * xStep, y1 + i * yStep))
        .toIterator

  override def toString(): String = s"(${x1}, ${y1}) -> (${x2}, ${y2})"

object Line:
  def fromString(s: String): Line =
    val dashIndex = s.indexOf('-')
    val first = s.substring(0, dashIndex - 1).nn.split(',').map(_.toInt)
    val second = s.substring(dashIndex + 3).nn.split(',').map(_.toInt)
    return Line(first(0), first(1), second(0), second(1))

class Grid():
  val pointCounts = HashMap[(Int, Int), Int]()

  def addPoint(x: Int, y: Int) =
    pointCounts.updateWith((x, y))(_ match
      case Some(i) => Some(i + 1)
      case None    => Some(1)
    )

  def pointsWithOverlap: Iterator[(Int, Int)] =
    for (k, v) <- pointCounts.iterator if v > 1 yield k
