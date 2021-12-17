package day17

import scala.collection.mutable.{Set => MSet}

// (x_min, x_max, y_min, y_max)
def exampleTarget = (20, 30, -10, -5)
def target = (60, 94, -171, -136)

def possibleYVelocityForYTarget(yMin: Int, yMax: Int): List[Int] =
  // the formula for height after n steps is (first step is step 1)
  // yVelocity * n - n * (n-1) / 2
  // the constraint yMin <= position after n steps <= yMax turns into
  // yMin / n + (n-1) / 2 <= yVelocity <= yMax / n + (n-1) / 2
  // for large enough n, there will be no integers between the left and right bits
  var yVelocities = MSet[Int]()
  LazyList
    .from(1)
    // map each n to whether or not there were any velocities at that n
    .map(n => {
      val minYVelocity = Math.ceil(yMin.toFloat / n + (n - 1).toFloat / 2).toInt
      val maxYVelocity =
        Math.floor(yMax.toFloat / n + (n - 1).toFloat / 2).toInt
      println(s"for n=${n}, ${minYVelocity} to ${maxYVelocity}")
      if minYVelocity <= maxYVelocity then
        (minYVelocity to maxYVelocity).foreach(yVelocities.add(_))
        true
      else false
    })
    // we want to stop when there have been two false in a row
    .scanLeft(0)((i, b) => if b then 0 else i + 1)
    .takeWhile(_ < 2)
    .force
  yVelocities.toList.sorted

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
  possibleYVelocityForYTarget(target._3, target._4)
    .map(highestPointForYVeloicty(_)._2)
    .max
