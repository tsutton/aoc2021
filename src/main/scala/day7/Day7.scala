package day7

import scala.io.Source

def input: Array[Int] =
  Source.fromResource("day7.txt").getLines.next.split(',').map(_.toInt)
def exampleInput: Array[Int] = "16,1,2,0,4,2,7,1,2,14".split(',').map(_.toInt)

def fuelToAlign(crabs: Seq[Int], position: Int): Int =
  crabs.map(crab => (crab - position).abs).sum

def part1: Int =
  val crabs = input
  val min = crabs.min
  val max = crabs.max
  (min to max)
    .map(position => (position, fuelToAlign(crabs, position)))
    .minBy(_._2)
    ._2

def nthTriangularNumber(n: Int) = n * (n + 1) / 2

def fuelToAlign2(crabs: Seq[Int], position: Int): Int =
  crabs.map(crab => nthTriangularNumber((crab - position).abs)).sum

def part2: Int =
  val crabs = input
  val min = crabs.min
  val max = crabs.max
  (min to max)
    .map(position => (position, fuelToAlign2(crabs, position)))
    .minBy(_._2)
    ._2
