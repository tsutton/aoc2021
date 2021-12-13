package day12

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable
import scala.io.Source

def exampleInput1: Iterator[String] = """start-A
start-b
A-c
A-b
b-d
A-end
b-end
""".linesIterator

def exampleInput2: Iterator[String] = """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
""".linesIterator

def input: Iterator[String] =
  Source.fromResource("day12.txt").getLines

def part1(): Int =
  val graph = Graph()
  for line <- input do
    val parts = line.split('-')
    graph.addEdge(parts(0), parts(1))
  // println(graph)
  graph.countPaths("start", "end")

def part2(): Int =
  val graph = Graph()
  for line <- input do
    val parts = line.split('-')
    graph.addEdge(parts(0), parts(1))
  println(graph)
  graph.countPathsPart2("start", "end")

class Graph:
  val edges: MMap[String, List[String]] = MMap()

  def addEdge(firstNode: String, secondNode: String) =
    val currentEdges = edges.getOrElse(firstNode, List())
    edges(firstNode) = currentEdges :+ secondNode

    val currentSecondEdges = edges.getOrElse(secondNode, List())
    edges(secondNode) = currentSecondEdges :+ firstNode

  def countPaths(startNode: String, endNode: String): Int =
    countPathsWithUsed(startNode, endNode, Set())

  def countPathsWithUsed(
      startNode: String,
      endNode: String,
      usedNodes: Set[String]
  ): Int =
    val nextNodes = edges.getOrElse(startNode, List())
    val count = nextNodes
      .filter(node => node(0).isUpper || !usedNodes.contains(node))
      .map(node => countPathsWithUsed(node, endNode, usedNodes + startNode))
      .sum
    if startNode == endNode then count + 1 else count

  def countPathsPart2(startNode: String, endNode: String): Int =
    countPathsWithUsedPart2(startNode, endNode, List(), false)

  def countPathsWithUsedPart2(
      startNode: String,
      endNode: String,
      usedNodes: List[String],
      visitedTwice: Boolean
  ): Int =
    if startNode == endNode then
      // println(usedNodes)
      return 1
    val nextNodes = edges.getOrElse(startNode, List())
    nextNodes
      .filter(_ != "start")
      .map(node => {
        val alreadyUsed = usedNodes.contains(node)
        if visitedTwice && node(0).isLower && alreadyUsed then 0
        else if node(0).isLower && alreadyUsed then
          countPathsWithUsedPart2(
            node,
            endNode,
            usedNodes :+ startNode,
            true
          )
        else
          countPathsWithUsedPart2(
            node,
            endNode,
            usedNodes :+ startNode,
            visitedTwice
          )
      })
      .sum

  override def toString(): String =
    var s = StringBuffer()
    for (k, v) <- edges do s.append(s"${k} -> ${v}\n")
    s.toString
