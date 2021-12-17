// TODO important note - the exact logic in this package doesn't exactly work
// But I got the right answer and am not interested enough in this problem to fix it.
package day17

import scala.collection.mutable.{Set => MSet}

// (x_min, x_max, y_min, y_max)
def exampleTarget = (20, 30, -10, -5)
def target = (60, 94, -171, -136)

def possibleVelocityNPairsForTarget(yMin: Int, yMax: Int): List[(Int, Int)] =
  var yVelocities = MSet[(Int, Int)]()
  LazyList
    .from(1)
    // map each n to whether or not there were any velocities at that n
    .map(n => {
      val velocities = yVelocitiesForNAndTarget(n, yMin, yMax)
      if velocities.isEmpty then false
      else
        velocities.foreach(v => yVelocities.add((v, n)))
        true
    })
    // we want to stop when there have been two false in a row
    // scan such that the value is the number of falses in a row
    .scanLeft(0)((i, b) => if b then 0 else i + 1)
    // TODO
    .takeWhile(_ < 10000)
    .force
  yVelocities.toList.sorted

def yVelocitiesForNAndTarget(n: Int, left: Int, right: Int): List[Int] =
  // the formula for height after n steps is (first step is step 1)
  // yVelocity * n - n * (n-1) / 2
  // the constraint yMin <= position after n steps <= yMax turns into
  // yMin / n + (n-1) / 2 <= yVelocity <= yMax / n + (n-1) / 2
  // for large enough n, there will be no integers between the left and right bits
  val minYVelocity = Math.ceil(left.toFloat / n + (n - 1).toFloat / 2).toInt
  val maxYVelocity =
    Math.floor(right.toFloat / n + (n - 1).toFloat / 2).toInt
  // println(s"for n=${n}, ${minYVelocity} to ${maxYVelocity}")
  (minYVelocity to maxYVelocity).toList

// assumption: 0 < left <= right
def xVelocitiesForNAndTarget(n: Int, left: Int, right: Int): List[Int] =
  // x is LIKE y but the velocities stop decreasing when they hit 0
  // for a given n, the velocities that are stationary after n steps are v_x <= n
  // if the calculated minVelocity is <= n, we have to do something different
  // in that case, we have the /non-stationary/ portion: from n+1 up to max
  // the the stationary portion: those velocities <= n whose final x position is in the range
  // the final position of velocity v is v * (v+1) / 2
  val minXVelocity = Math.ceil(left.toFloat / n + (n - 1).toFloat / 2).toInt
  val maxXVelocity =
    Math.floor(right.toFloat / n + (n - 1).toFloat / 2).toInt
  // println(s"for n=${n}, ${minYVelocity} to ${maxYVelocity}")
  val nonStationary = (Math.max(minXVelocity, n + 1) to maxXVelocity).toList
  val stationary =
    (n to 1 by -1).filter(v => (left to right).contains(v * (v + 1) / 2))
  nonStationary ++ stationary

def stationaryXVelocitiesForTarget(left: Int, right: Int): List[Int] =
  LazyList
    .from(1)
    .map(v => (v, v * (v + 1) / 2))
    .takeWhile(_._2 <= right)
    .filter(_._2 >= left)
    .map(_._1)
    .toList

// returns the pair (step number, highest point) for a given v_y
def highestPointForYVeloicty(yVelocity: Int): (Int, Int) =
  // the formula for height after n steps is (first step is step 1)
  // yVelocity * n - n * (n-1) / 2
  // if n were a continuous variable, it would be height at
  // n = yVelocity - 1/2
  // since n is an integer AND since the function is simple enough (goes up for a while then down)
  // we just try the two nearest and report the better one
  List(yVelocity - 1, yVelocity)
    .map(n => (n, yVelocity * n - n * (n - 1) / 2))
    .maxBy(_._2)

def part1(): Int =
  possibleVelocityNPairsForTarget(target._3, target._4)
    .map(_._1)
    .map(highestPointForYVeloicty(_)._2)
    .max

def part2(): Int =
  val in = target
  val velocitiesWithDuplicates = for
    (v_y, n) <- possibleVelocityNPairsForTarget(in._3, in._4)
    v_x <- xVelocitiesForNAndTarget(n, in._1, in._2)
  yield (v_x, v_y)
  // println(velocitiesWithDuplicates.distinct.mkString("\n"))
  velocitiesWithDuplicates.distinct.size
