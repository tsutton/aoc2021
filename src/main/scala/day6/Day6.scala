package day6

import scala.io.Source
import scala.collection.mutable.{ArraySeq}

def input: String = Source.fromResource("day6.txt").getLines.next
def exampleInput: String = "3,4,3,1,2"

def part1: Long =
  doTheThing(input, 80)

def part2: Long =
  doTheThing(input, 256)

def doTheThing(input: String, days: Int) =
  val school = SchoolOfFish()
  for i <- input.split(',').map(_.toInt) do school.addFish(i)
  school.passNDays(days)
  school.size

class SchoolOfFish():
  var countsOfFishByTimer: ArraySeq[Long] = ArraySeq.fill[Long](9)(0)

  def addFish(timer: Int) =
    countsOfFishByTimer(timer) += 1

  def passOneday() =
    val newCounts = ArraySeq.fill[Long](9)(0)
    for i <- 0 to 7 do newCounts(i) = countsOfFishByTimer(i + 1)
    newCounts(8) = countsOfFishByTimer(0)
    newCounts(6) += countsOfFishByTimer(0)
    countsOfFishByTimer = newCounts

  def passNDays(n: Int) = (0 until n).foreach(_ => passOneday())

  def size: Long = countsOfFishByTimer.sum
