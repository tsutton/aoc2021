package day20

import scala.io.Source
import day20.SparseGrid

def exampleInput: Iterator[String] =
  """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
""".linesIterator

def input: Iterator[String] = Source.fromResource("day20.txt").getLines

def parse(input: Iterator[String]): (Array[Bit], Grid) =
  val arr = input.next.map(Bit.fromChar).toArray
  input.next
  val grid = input.map(line => line.map(Bit.fromChar).toArray).toArray
  (arr, Grid(grid))

enum Bit:
  case Light, Dark

  def toChar: Char = this match
    case Light => '#'
    case Dark  => '.'

  def toDigit: Int = this match
    case Light => 1
    case Dark  => 0

object Bit:
  def fromChar(c: Char): Bit = c match
    case '#' => Light
    case '.' => Dark

class Grid(val grid: Array[Array[Bit]], val outerBit: Bit = Bit.Dark):
  def width = grid(0).length
  def height = grid.length

  def enhance(key: Array[Bit]): Grid =
    val newOuterBit = if outerBit == Bit.Dark then key(0) else key(511)
    val newGrid =
      (-1 to height)
        .map(y => (-1 to width).map(x => key(squareToBinary(x, y))).toArray)
        .toArray
    Grid(newGrid, newOuterBit)

  def lookup(x: Int, y: Int): Bit =
    if x < 0 || x >= width || y < 0 || y >= height then outerBit
    else grid(y)(x)

  def squareToBinary(x: Int, y: Int): Int =
    val bits = for
      yi <- y - 1 to y + 1
      xi <- x - 1 to x + 1
    yield lookup(xi, yi)
    bits.reverse.zipWithIndex.map((b, i) => b.toDigit * 1 << i).sum

  def litSquareCount: Int =
    grid.map(row => row.count(bit => bit == Bit.Light)).sum

  override def toString =
    (-1 to height)
      .map(y => (-1 to width).map(x => lookup(x, y).toChar).mkString)
      .mkString("\n")

def part1(): Int =
  val (key, grid) = parse(input)
  // println(grid)
  grid.enhance(key).enhance(key).litSquareCount
// ???

class SparseGrid(
    val litSquares: Set[(Int, Int)],
    val width: Int,
    val height: Int,
    val outerBit: Bit = Bit.Dark
):
  def enhance(key: Array[Bit]): SparseGrid =
    val newOuterBit = if outerBit == Bit.Dark then key(0) else key(511)
    val newGrid = for
      y <- -1 to height
      x <- -1 to width
      if key(squareToBinary(x, y)) == Bit.Light
    yield (x + 1, y + 1)
    new SparseGrid(newGrid.toSet, width + 2, height + 2, newOuterBit)

  def lookup(x: Int, y: Int): Bit =
    if x < 0 || x >= width || y < 0 || y >= height then outerBit
    else if litSquares.contains((x, y)) then Bit.Light
    else Bit.Dark

  def squareToBinary(x: Int, y: Int): Int =
    val bits = for
      yi <- y - 1 to y + 1
      xi <- x - 1 to x + 1
    yield lookup(xi, yi)
    bits.reverse.zipWithIndex.map((b, i) => b.toDigit * 1 << i).sum

  def litSquareCount: Int =
    litSquares.size

  override def toString =
    (-1 to height)
      .map(y => (-1 to width).map(x => lookup(x, y).toChar).mkString)
      .mkString("\n")

object SparseGrid:
  def apply(s: Set[(Int, Int)]): SparseGrid =
    val width = s.maxBy(_._1)._1
    val height = s.maxBy(_._2)._2
    new SparseGrid(s, width + 2, height + 2)

def parseSparse(input: Iterator[String]): (Array[Bit], SparseGrid) =
  val arr = input.next.map(Bit.fromChar).toArray
  input.next
  val litSquares = for
    (line, y) <- input.zipWithIndex
    (c, x) <- line.zipWithIndex
    if c == '#'
  yield (x, y)
  (arr, SparseGrid(litSquares.toSet))

def part2(): Int =
  //  actually is slower with parse sparse :(
  var (key, grid) = parse(input)
  (1 to 50).foreach(_ => grid = grid.enhance(key))
  grid.litSquareCount
