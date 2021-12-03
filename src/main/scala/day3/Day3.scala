package day3

import scala.io.Source
import scala.collection.mutable

def input: Iterator[String] = Source.fromResource("day3.txt").getLines

def exampleInput = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010""".linesIterator

def part1: Int =
  // digit number (0 = leftmost) maps to (num of 0s so far, num of 1s so far) in that digit
  var countsByIdx = mutable.Map[Int, (Int, Int)]()
  for
    line <- input
    (ch, idx) <- line.zipWithIndex
  do
    val chInt = if ch == '0' then 0 else 1
    print(chInt)
    countsByIdx.updateWith(idx)(if chInt == 1 then incSecond else incFirst)
  val gammaList =
    for (idx, (zeros, ones)) <- countsByIdx yield if zeros > ones then 0 else 1
  val epsilonList =
    for (idx, (zeros, ones)) <- countsByIdx yield if zeros > ones then 1 else 0
  listToDecimal(gammaList.toList) * listToDecimal(epsilonList.toList)

def incFirst(o: Option[(Int, Int)]): Option[(Int, Int)] = o match
  case None         => Some((1, 0))
  case Some((a, b)) => Some((a + 1, b))

def incSecond(o: Option[(Int, Int)]): Option[(Int, Int)] = o match
  case None         => Some((0, 1))
  case Some((a, b)) => Some((a, b + 1))

def listToDecimal(l: List[Int]): Int =
  val len = l.length
  l.zipWithIndex.map(v => v._1 * 1 << (len - v._2 - 1)).sum

def part2: Int =
  val oxygen = part2FilterAll(input.toList, true)
  val co2 = part2FilterAll(input.toList, false)
  oxygen * co2

// oxygen: mostCommon = true
// co2: mostCommon = false
def part2FilterOnce(
    l: List[String],
    bitIdx: Int,
    mostCommon: Boolean
): List[String] =
  val groupedByDigit = l.groupBy(_(bitIdx))
  if groupedByDigit.size == 1 then return groupedByDigit.head._2
  val (lessCommonDigit, moreCommonDigit) =
    if (groupedByDigit.get('0').map(_.size) getOrElse 0) > (groupedByDigit
        .get(
          '1'
        )
        .map(_.size) getOrElse 0)
    then ('1', '0')
    else ('0', '1')
  if mostCommon then groupedByDigit(moreCommonDigit)
  else groupedByDigit(lessCommonDigit)

// oxygen: mostCommon = true
// co2: mostCommon = false
def part2FilterAll(it: List[String], mostCommon: Boolean): Int =
  var filtered = it
  val len = filtered(0).length
  for i <- 0 until len do filtered = part2FilterOnce(filtered, i, mostCommon)
  assert(filtered.size == 1)
  listToDecimal(
    filtered(0).map(c => if c == '0' then 0 else 1).toList
  )
