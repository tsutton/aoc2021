package day14

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable

def input: Iterator[String] =
  Source.fromResource("day14.txt").getLines

def exampleInput: Iterator[String] = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
""".linesIterator

def parse(iter: Iterator[String]): (String, HashMap[String, Char]) =
  val template = iter.next
  iter.next // skip blank
  val rules = HashMap[String, Char]()
  for line <- iter do
    val parts = line.split(" -> ").nn
    rules(parts(0).nn) = parts(1).nn(0)
  (template, rules)

def step(template: String, rules: HashMap[String, Char]): String =
  val builder = mutable.StringBuilder()
  builder += template(0)
  for pair <- template.sliding(2) do
    rules.get(pair).foreach(builder += _)
    builder += pair(1)
  builder.result()

def part1(): Int =
  var (i, r) = parse(input)
  for j <- 0 until 10 do i = step(i, r)
  // val counts = i.groupBy((c: Char) => c)
  val counts = HashMap[Char, Int]()
  for c <- i do
    counts.get(c) match
      case Some(i) => counts(c) = i + 1
      case None    => counts(c) = 1
  counts.maxBy(_(1)).apply(1) - counts.minBy(_(1)).apply(1)

def step2(
    counts: Map[String, Long],
    rules: HashMap[String, Char]
): Map[String, Long] =
  val newCounts = HashMap[String, Long]()
  for (pair, count) <- counts do
    val newChar = rules(pair)
    val leftPair = s"${pair(0)}${newChar}"
    val rightPair = s"${newChar}${pair(1)}"
    if !newCounts.contains(leftPair) then newCounts(leftPair) = 0
    if !newCounts.contains(rightPair) then newCounts(rightPair) = 0
    newCounts(leftPair) += count
    newCounts(rightPair) += count
  newCounts.toMap

def part2(): Long =
  val (i, r) = parse(input)
  var hmap = HashMap[String, Long]()
  for pair <- i.sliding(2) do hmap(pair) = hmap.get(pair).getOrElse(0L) + 1
  var map = hmap.toMap
  for j <- 0 until 40 do
    // println(map)
    // println(map.values.sum)
    map = step2(map, r)
  val charCounts = HashMap[Char, Long]()
  for (k, v) <- map do
    charCounts.updateWith(k(1))(_ match
      case Some(c) => Some(c + v)
      case None    => Some(v)
    )
  // println(charCounts)
  // println(i)
  charCounts(i(0)) += 1L
  println(charCounts)
  println(charCounts.values.sum)
  charCounts.maxBy(_(1)).apply(1) - charCounts.minBy(_(1)).apply(1)

/*
HashMap(
C -> 3419841458483
S -> 563180205073
P -> 1052649059445
B -> 2204646124337
F -> 2309811056934
V -> 2450552584909
H -> 2320589580428
K -> 882413270349
N -> 2867584904413
O -> 1719941055598
FNFPPNKPPHSOKFFHOFOC
 */
