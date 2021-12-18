package day18

import scala.io.Source

case class Pair(var left: Int | Pair, var right: Int | Pair):
  def add(other: Pair): Pair =
    val p = Pair(this, other)
    p.reduce()
    p

  def reduce(): Unit =
    if explode() then reduce()
    else if split() then reduce()

  def explode(): Boolean = explodeAtDepth(0)._1

  def explodeAtDepth(depth: Int): (Boolean, Option[Int], Option[Int]) =
    (depth, left, right) match
      case (3, Pair(a: Int, b: Int), _) => {
        left = 0
        pushLeftInRightSubtree(b)
        return (true, Some(a), None)
      }
      case (3, _, Pair(a: Int, b: Int)) => {
        right = 0
        pushRightInLeftSubtree(a)
        return (true, None, Some(b))
      }
      case _ => {}
    left match
      case p: Pair => {
        p.explodeAtDepth(depth + 1) match
          case (true, Some(a), x) => return (true, Some(a), x)
          case (true, x, Some(a)) => {
            pushLeftInRightSubtree(a)
            return (true, None, None)
          }
          case (true, x, y) => return (true, x, y)
          case _            => {}
      }
      case _ => {}
    right match
      case p: Pair => {
        p.explodeAtDepth(depth + 1) match
          case (true, x, Some(a)) => return (true, x, Some(a))
          case (true, Some(a), x) => {
            pushRightInLeftSubtree(a)
            return (true, None, None)
          }
          case (true, x, y) => return (true, x, y)
          case _            => {}
      }
      case _ => {}
    (false, None, None)

  def split(): Boolean =
    left match
      case i: Int if i >= 10 => {
        left = Pair(i / 2, (i + 1) / 2)
        return true
      }
      case p: Pair => if p.split() then return true
      case _       => {}
    right match
      case i: Int if i >= 10 => {
        right = Pair(i / 2, (i + 1) / 2)
        return true
      }
      case p: Pair => if p.split() then return true else return false
      case _       => return false

  def pushRight(value: Int): Unit = right match
    case i: Int  => right = i + value
    case r: Pair => r.pushRight(value)

  def pushRightInLeftSubtree(value: Int) = left match
    case i: Int  => left = i + value
    case l: Pair => l.pushRight(value)

  def pushLeft(value: Int): Unit = left match
    case i: Int  => left = i + value
    case l: Pair => l.pushLeft(value)

  def pushLeftInRightSubtree(value: Int): Unit = right match
    case i: Int  => right = i + value
    case r: Pair => r.pushLeft(value)

  def magnitude: Int =
    val leftMagnitude = left match
      case i: Int  => 3 * i
      case p: Pair => 3 * p.magnitude

    val rightMagnitude = right match
      case i: Int  => 2 * i
      case p: Pair => 2 * p.magnitude

    leftMagnitude + rightMagnitude

def parsePair(source: String, startIdx: Int): (Pair, Int) =
  // println(s"parsing pair in ${source.substring(startIdx)}")
  assert(source(startIdx) == '[')
  var currentIdx = startIdx + 1
  val left: Int | Pair = source(currentIdx) match
    case '[' => {
      val (leftTmp, currentTmp) = parsePair(source, currentIdx)
      assert(source(currentTmp) == ',')
      currentIdx = currentTmp + 1
      leftTmp
    }
    case _ => {
      val commaIdx = source.indexOf(',', currentIdx)
      // println(s"parsing int from ${source.substring(currentIdx, commaIdx)}")
      val leftTmp = Integer.parseInt(source.substring(currentIdx, commaIdx))
      assert(source(commaIdx) == ',')
      currentIdx = commaIdx + 1
      leftTmp
    }
  // println(
  //   s"got left=${left}, parsing right from ${source.substring(currentIdx)}"
  // )
  val right: Int | Pair = source(currentIdx) match
    case '[' => {
      val (leftTmp, currentTmp) = parsePair(source, currentIdx)
      assert(source(currentTmp) == ']')
      currentIdx = currentTmp + 1
      leftTmp
    }
    case _ => {
      val endIdx = source.indexOf(']', currentIdx)
      val leftTmp = Integer.parseInt(source.substring(currentIdx, endIdx))
      assert(source(endIdx) == ']')
      currentIdx = endIdx + 1
      leftTmp
    }
  assert(source(currentIdx - 1) == ']')
  // println(s"got right=${right}")
  (Pair(left, right), currentIdx)

def input: Iterator[String] = Source.fromResource("day18.txt").getLines

def exampleInput: Iterator[String] =
  """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
""".linesIterator

def part1(): Int =
  input.map(parsePair(_, 0)._1).reduce((a, b) => a.add(b)).magnitude

def part2(): Int =
  val strList = input.toSeq
  var maxFound = -1
  for
    i <- 0 until strList.length
    j <- 0 until strList.length
    if i != j
  do
    val left = parsePair(strList(i), 0)._1
    val right = parsePair(strList(j), 0)._1
    val c = left.add(right)
    // println(s"${i} + ${j} => ${c.magnitude}")
    maxFound = Math.max(maxFound, c.magnitude)
  maxFound
