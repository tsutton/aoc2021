package day11

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.LazyList
import scala.compiletime.ops.boolean

def input: Iterator[String] =
  Source.fromResource("day11.txt").getLines

def exampleInput: Iterator[String] = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
""".linesIterator

def part1(): Int =
  val grid = OctopusGrid.parse(input)
  var flashes = 0
  for i <- 0 to 99 do
    // println(grid)
    // println()
    flashes += grid.step()
  flashes

def part2(): Int =
  val grid = OctopusGrid.parse(input)
  LazyList
    .from(1)
    .dropWhile(_ => {
      grid.step()
      !grid.allZeroes
    })
    .head

class OctopusGrid(val grid: ArrayBuffer[ArrayBuffer[Int]]):
  // step the grid, returning the number of flashes
  def step(): Int =
    // strategy: increase all by one
    // then, repeat the following until nothing happens
    // - for each >9, set it to 0, increment the its numbers that are > 0 by 1,
    //   and increment the number of flashes by 1
    for
      row <- 0 to 9
      col <- 0 to 9
    do grid(row)(col) += 1
    var totalFlashes = 0
    var flashes = flash()
    while flashes > 0 do
      totalFlashes += flashes
      flashes = flash()
    totalFlashes

  def allZeroes: Boolean =
    for
      row <- 0 to 9
      col <- 0 to 9
      if grid(row)(col) != 0
    do return false
    return true

  private def flash(): Int =
    var flashes = 0
    for
      row <- 0 to 9
      col <- 0 to 9
      if grid(row)(col) > 9
    do
      flashes += 1
      grid(row)(col) = 0
      for
        d_row <- -1 to 1
        d_col <- -1 to 1
        if (d_row, d_col) != (0, 0)
        if row + d_row >= 0 && row + d_row <= 9
        if col + d_col >= 0 && col + d_col <= 9
        if grid(row + d_row)(col + d_col) != 0
      do grid(row + d_row)(col + d_col) += 1
    flashes

  override def toString(): String =
    grid.map(_.mkString("")).mkString("\n")

object OctopusGrid:
  def parse(value: Iterator[String]): OctopusGrid =
    OctopusGrid(
      ArrayBuffer.from(
        value.map(line => ArrayBuffer.from(line.map(_.toString.toInt)))
      )
    )
