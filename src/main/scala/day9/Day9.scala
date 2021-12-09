package day9

import scala.io.Source
import scala.collection.mutable.{ArrayDeque, Set => MSet}

def input: Iterator[String] =
  Source.fromResource("day9.txt").getLines

def exampleInput: Iterator[String] = """2199943210
3987894921
9856789892
8767896789
9899965678
""".linesIterator

def parse: Vector[Vector[Int]] =
  input
    .map(line => line.toVector.map(char => char.toString.toInt))
    .toVector

def part1: Int =
  val in = parse
  val width = in(0).size
  val height = in.size
  val lows = for
    row <- 0 until height
    col <- 0 until width
    if (row == 0 || in(row)(col) < in(row - 1)(col))
    if (row == height - 1 || in(row)(col) < in(row + 1)(col))
    if (col == 0 || in(row)(col) < in(row)(col - 1))
    if (col == width - 1 || in(row)(col) < in(row)(col + 1))
  yield
  // println(s"(${row}, ${col}): ${in(row)(col)}")
  in(row)(col) + 1
  lows.sum

def part2: Int =
  val in = parse
  val width = in(0).size
  val height = in.size
  val lows = for
    row <- 0 until height
    col <- 0 until width
    if (row == 0 || in(row)(col) < in(row - 1)(col))
    if (row == height - 1 || in(row)(col) < in(row + 1)(col))
    if (col == 0 || in(row)(col) < in(row)(col - 1))
    if (col == width - 1 || in(row)(col) < in(row)(col + 1))
  yield (row, col)
  val basins = lows.map(low => basin(in, low).size).sorted
  // println(basins)
  basins.slice(basins.size - 3, basins.size).product

def basin(grid: Vector[Vector[Int]], low: (Int, Int)): Set[(Int, Int)] =
  val width = grid(0).size
  val height = grid.size
  val queue = ArrayDeque(low)
  val found = MSet[(Int, Int)]()
  // println(s"basin of ${low}, height=${height}, width=${width}")
  while !queue.isEmpty do
    val (row, col) = queue.remove(0)
    found.add((row, col))
    for
      (dx, dy) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
      if row + dy >= 0 && row + dy < height
      if col + dx >= 0 && col + dx < width
      if grid(row + dy)(col + dx) != 9
      if !found.contains((row + dy, col + dx))
    do queue.append((row + dy, col + dx))
  found.toSet
