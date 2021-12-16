package day15

import scala.io.Source
import scala.collection.mutable.{Set => MSet, Map => MMap, PriorityQueue}
import scala.collection.mutable.TreeMap

def input: Iterator[String] =
  Source.fromResource("day15.txt").getLines

def exampleInput: Iterator[String] = """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
""".linesIterator

def parse(it: Iterator[String]): Map[(Int, Int), Int] =
  val pairs = for
    (line, row) <- it.zipWithIndex
    (c, col) <- line.zipWithIndex
  yield ((row, col), c.asDigit)
  pairs.toMap

def part1(): Int =
  val weights = parse(input)
  val size = input.next.size
  // Dijkstra's algorithm: computes the shortest distance from a given source node to all other nodes
  // but we can short-circuit once we find the end node
  val visited = MSet[(Int, Int)]()
  val bestDistances = MMap[(Int, Int), Int](
    (0, 0) -> 0
  )
  while !visited.contains((size - 1, size - 1)) do
    val ((row, col), distance) =
      bestDistances.filterKeys(!visited.contains(_)).minBy(_(1))
    println(s"visiting ${(row, col)} at distance ${distance}")
    visited.addOne((row, col))
    for
      (d_row, d_col) <- List((-1, 0), (1, 0), (0, 1), (0, -1))
      if row + d_row >= 0 && row + d_row < size
      if col + d_col >= 0 && col + d_col < size
    do
      println(s"inspecting ${(row + d_row, col + d_col)}")
      val min_from_current = distance + weights((row + d_row, col + d_col))
      bestDistances.get((row + d_row, col + d_col)) match
        case None =>
          bestDistances((row + d_row, col + d_col)) = min_from_current
        case Some(i) if i > min_from_current =>
          bestDistances((row + d_row, col + d_col)) = min_from_current
        case _ => {}
  bestDistances((size - 1, size - 1))

def part2GetWeight(
    tileWeights: Map[(Int, Int), Int],
    size: Int,
    position: (Int, Int)
): Int =
  val (row, col) = position
  val baseWeight = tileWeights(row % size, col % size)
  val btwnZeroAndEight =
    (baseWeight + (row - row % size) / size + (col - col % size) / size) % 9
  if btwnZeroAndEight == 0 then 9 else btwnZeroAndEight

def part2(): Int =
  val weights = parse(input)
  val size = input.next.size
  // Dijkstra's algorithm: computes the shortest distance from a given source node to all other nodes
  // but we can short-circuit once we find the end node
  val visited = MSet[(Int, Int)]()
  val bestDistances = MMap[(Int, Int), Int](
    (0, 0) -> 0
  )
  // frontier means nodes that are unvisited but have a best distance
  // unfortunately there's no real priority queue of the type needed for dijkstra's in stdlib
  // scala.collections.mutable.PriorityQueue doesn't have a way to change the priority for a given key
  // The best bet would be to use TreeMap - a map that's sorted by key - mapping priorities
  // to a list of squares with that priority.
  val frontier = MSet[(Int, Int)]((0, 0))
  while !visited.contains((5 * size - 1, 5 * size - 1)) do
    val ((row, col), distance) =
      frontier.map(node => (node, bestDistances(node))).minBy(_(1))
    // println(s"visiting ${(row, col)} at distance ${distance}")
    visited.addOne((row, col))
    frontier.remove((row, col))
    for
      (d_row, d_col) <- List((-1, 0), (1, 0), (0, 1), (0, -1))
      if row + d_row >= 0 && row + d_row < 5 * size
      if col + d_col >= 0 && col + d_col < 5 * size
    do
      // println(s"inspecting ${(row + d_row, col + d_col)}")
      val neighbor = (row + d_row, col + d_col)
      val min_from_current =
        distance + part2GetWeight(weights, size, neighbor)
      bestDistances.get(neighbor) match
        case None => {
          bestDistances(neighbor) = min_from_current
          if !visited.contains(neighbor) then frontier.add(neighbor)
        }
        case Some(i) if i > min_from_current =>
          bestDistances(neighbor) = min_from_current
        case _ => {}
  bestDistances((5 * size - 1, 5 * size - 1))

class MyPriorityQueue():
  val byPriority = TreeMap[Int, MSet[(Int, Int)]]()
  val priorities = MMap[(Int, Int), Int]()
  def add_with_priority(value: (Int, Int), priority: Int) =
    byPriority.updateWith(priority)(_ match
      case None => Some(MSet(value))
      case Some(l) => {
        l += value
        Some(l)
      }
    )
    priorities(value) = priority

  def pop_min(): ((Int, Int), Int) =
    val (minPriority, minList) = byPriority.head
    val newMinList = minList.tail
    if newMinList.isEmpty then byPriority.remove(minPriority)
    else byPriority(minPriority) = newMinList
    (minList.head, minPriority)

  private def remove(value: (Int, Int)): Unit =
    if !priorities.contains(value) then return
    val currentPriority = priorities(value)
    val currentList = byPriority(currentPriority)
    currentList -= value
    if currentList.isEmpty then byPriority.remove(currentPriority)
    else byPriority(currentPriority) = currentList

  def set_priority(value: (Int, Int), newPriority: Int) =
    remove(value)
    add_with_priority(value, newPriority)

// runs in 1-2 seconds, rather than the 19 seconds of part2
def part2Faster(): Int =
  val weights = parse(input)
  val size = input.next.size
  // Dijkstra's algorithm: computes the shortest distance from a given source node to all other nodes
  // but we can short-circuit once we find the end node
  val visited = MSet[(Int, Int)]()
  val bestDistances = MMap[(Int, Int), Int](
    (0, 0) -> 0
  )
  val queue = MyPriorityQueue()
  queue.add_with_priority((0, 0), 0)
  while !visited.contains((5 * size - 1, 5 * size - 1)) do
    val ((row, col), distance) =
      queue.pop_min()
    // println(s"visiting ${(row, col)} at distance ${distance}")
    visited.addOne((row, col))
    for
      (d_row, d_col) <- List((-1, 0), (1, 0), (0, 1), (0, -1))
      if row + d_row >= 0 && row + d_row < 5 * size
      if col + d_col >= 0 && col + d_col < 5 * size
    do
      // println(s"inspecting ${(row + d_row, col + d_col)}")
      val neighbor = (row + d_row, col + d_col)
      val min_from_current =
        distance + part2GetWeight(weights, size, neighbor)
      bestDistances.get(neighbor) match
        case None =>
          bestDistances(neighbor) = min_from_current
          queue.set_priority(neighbor, min_from_current)
        case Some(i) if i > min_from_current =>
          bestDistances(neighbor) = min_from_current
          queue.set_priority(neighbor, min_from_current)
        case _ => {}
  bestDistances((5 * size - 1, 5 * size - 1))
