package day4

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Seq => MSeq}

def input: Iterator[String] = Source.fromResource("day4.txt").getLines
def exampleInput: Iterator[String] =
  """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
""".linesIterator

val boardSize = 5

def parse: (Array[Int], ArrayBuffer[Board]) =
  val it = input
  val calls = it.next.split(',').map(_.toInt)
  val boards = ArrayBuffer[Board]()
  while it.hasNext do
    it.next
    boards.addOne(Board.fromLines(it.take(boardSize).toList))
  (calls, boards)

def part1: Int =
  val (calls, boards) = parse
  for
    call <- calls
    board <- boards
  do
    board.mark(call.toInt)
    if board.is_bingo then return board.sum_of_unmarked * call.toInt
  ???

def part2: Int =
  val (calls, boards) = parse
  var nBingos = 0
  for
    call <- calls
    board <- boards
    if !board.is_bingo
  do
    board.mark(call.toInt)
    if board.is_bingo then
      nBingos += 1
      if nBingos == boards.size then return board.sum_of_unmarked * call.toInt
  ???

class Board(grid: Seq[Seq[Int]]):
  val markedSquares: Array[Array[Boolean]] =
    Array.fill(boardSize, boardSize)(false)
  def mark(value: Int) =
    for (x, y) <- find(value) do markedSquares(x)(y) = true
  def find(value: Int): Option[(Int, Int)] =
    for
      i <- 0 until boardSize
      j <- 0 until boardSize
    do if grid(i)(j) == value then return Some((i, j))
    None
  def is_bingo: Boolean =
    for i <- 0 until boardSize do
      if (0 until boardSize).map(markedSquares(i)(_)).reduce(_ && _) then
        return true
      if (0 until boardSize).map(markedSquares(_)(i)).reduce(_ && _) then
        return true
    false
  def sum_of_unmarked: Int =
    val marked = for
      i <- 0 until boardSize
      j <- 0 until boardSize
      if !markedSquares(i)(j)
    yield grid(i)(j)
    marked.sum

  override def toString: String =
    grid.map(_.mkString(" ")).mkString("\n") + "\n" + markedSquares
      .map(_.map(if _ then "x" else "-").mkString(" "))
      .mkString("\n")

object Board:
  def fromLines(lines: List[String]): Board =
    Board(lines.map(_.split(' ').filter(_ != "").map(_.toInt)))
