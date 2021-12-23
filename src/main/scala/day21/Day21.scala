package day21

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MMap}

def exampleInput: Iterator[String] = """Player 1 starting position: 4
Player 2 starting position: 8
""".linesIterator

def input: Iterator[String] = """Player 1 starting position: 6
Player 2 starting position: 8
""".linesIterator

def parse(in: Iterator[String]): (Int, Int) =
  (
    Integer.parseInt(in.next.last.toString),
    Integer.parseInt(in.next.last.toString)
  )

def part1(): Int =
  var (p1Position, p2Position) = parse(input)
  var positions = ArrayBuffer(p1Position, p2Position)
  var scores = ArrayBuffer(0, 0)
  for i <- LazyList.from(1) do
    val idx = 1 - i % 2
    positions(idx) = (positions(idx) + (3 * i - 1) * 3) % 10
    if positions(idx) == 0 then positions(idx) = 10
    scores(idx) += positions(idx)
    // println(s"${i}: ${scores}")
    if scores(idx) >= 1000 then return scores(1 - idx) * i * 3
  ???

def part2(): Long =
  var (p1Position, p2Position) = parse(input)
  val solver = Part2Solver()
  val (x, y) = solver.solve(Position(0, p1Position, 0, p2Position, 1))
  x.max(y)

case class Position(
    val p1Score: Int,
    val p1Position: Int,
    val p2Score: Int,
    val p2Position: Int,
    val turn: Int
)

class Part2Solver():
  // (p1 score, p1 position, p2 score, p2 position, player whose turn it is) => (p1 wins universes, p2 wins universes)
  val universes: MMap[Position, (Long, Long)] = MMap()

  val rollSumsToUniverses: Map[Int, Long] = Map(
    3 -> 1L,
    4 -> 3L,
    5 -> 6L,
    6 -> 7L,
    7 -> 6L,
    8 -> 3L,
    9 -> 1L
  )

  def solve(
      position: Position
  ): (Long, Long) =
    if universes.contains(position) then return universes(position)
    val sol =
      if position.turn == 1 then
        rollSumsToUniverses.keysIterator
          .map(roll => {
            val universes = rollSumsToUniverses(roll)
            val pos = newPosition(position.p1Position, roll)
            val newScore = position.p1Score + pos
            if newScore >= 21 then (universes, 0L)
            else
              val (x, y) = solve(
                position.copy(p1Position = pos, p1Score = newScore, turn = 2)
              )
              (universes * x, universes * y)
          })
          .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
      else
        rollSumsToUniverses.keysIterator
          .map(roll => {
            val universes = rollSumsToUniverses(roll)
            val pos = newPosition(position.p2Position, roll)
            val newScore = position.p2Score + pos
            if newScore >= 21 then (0L, universes)
            else
              val (x, y) = solve(
                position.copy(p2Position = pos, p2Score = newScore, turn = 1)
              )
              (universes * x, universes * y)
          })
          .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    universes(position) = sol
    sol

def newPosition(currentPosition: Int, roll: Int): Int =
  val newPos = (currentPosition + roll) % 10
  if newPos == 0 then 10 else newPos
