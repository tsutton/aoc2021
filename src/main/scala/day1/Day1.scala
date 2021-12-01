package day1

import scala.io.Source

def input: List[String] =
  Source.fromResource("day1.txt").getLines.toList

def part1: Int =
  val i = input
  var previous = input.head.toInt.nn
  var count = 0;
  for l <- input do
    val next = l.toInt.nn
    if next > previous then count += 1
    previous = next
  count

def part2: Int =
  val it =
    Source
      .fromResource("day1.txt")
      .getLines
      .map(_.toInt.nn)
      .sliding(3, 1)
      .map(_.sum)
  var previous = it.next
  var count = 0;
  for l <- it do
    val next = l
    if next > previous then count += 1
    previous = next
  count
