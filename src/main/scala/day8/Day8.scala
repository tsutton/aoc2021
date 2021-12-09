package day8

import scala.io.Source

def input: Iterator[String] =
  Source.fromResource("day8.txt").getLines

def exampleInput: Iterator[String] =
  """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
""".linesIterator

def part1: Int =
  val in = input
  in.map(line =>
    line
      .split('|')(1)
      .stripLeading
      .nn
      .split(' ')
      .map(_.length)
      .filter(digitLength =>
        digitLength match
          case 2 | 3 | 4 | 7 => true
          case _             => false
      )
      .size
  ).sum

def part2: Int =
  input.map(solveLine).sum

def solveLine(line: String): Int =
  val parts = line.split('|')
  val inputDigits = parts(0).strip.nn.split(' ')
  val outputDigits = parts(1).strip.nn.split(' ')
  val allDigits = inputDigits ++ outputDigits
  // Sort as follows:
  // digits with two chars, digits with 6 chars, digits with 3 chars
  // digits with 4 chars, digits with 5 chars, digits with 7 chars
  // this gives us an approximate order that controls the branching
  val digitOrders = List(2, 6, 3, 4, 5, 7)
  val sortedDigits =
    allDigits.sortBy(digit => digitOrders.indexOf(digit.length))
  // sortedDigits.foreach(println(_))
  val mapping = findMapping(Map(), sortedDigits).get
  // println(mapping)
  outputDigits
    .map(digit => segmentsToDigit(mapping, digit))
    .zipWithIndex
    .map((d, i) => d * math.pow(10, 3 - i))
    .sum
    .toInt

// Our process for solving is recursive.
// In the base case, currentMapping is empty, since we aren't trying anything yet, and
// remainingDigits is the whole list of sequences in the input.
// (Here "sequence" is a bit like "cfbegad").
// For each sequence, we figure out which possible digits it can be based on its length,
// and for each try to assign that sequence to that
// digit by calling findMappingWithDigit. If such a call fails, we try assigning that sequence
// to the next digit.
// Then findMappingWithDigit will recursively call this function when it moves on to the next
// sequence.
def findMapping(
    currentMapping: Map[Char, Char],
    remainingSequences: Seq[String]
): Option[Map[Char, Char]] =
  if remainingSequences.isEmpty then return Some(currentMapping)
  val s = remainingSequences.head
  val lengthToDigits = Map(
    2 -> List(1),
    6 -> List(0, 6, 9),
    3 -> List(7),
    4 -> List(4),
    5 -> List(2, 3, 5),
    7 -> List(8)
  )
  for digit <- lengthToDigits(s.length) do
    // println(s"trying to match ${s} as ${digit}")
    val attempt =
      findMappingWithDigit(currentMapping, remainingSequences, digit)
    if !attempt.isEmpty then return attempt
  return None

val digitToExpected = Map(
  0 -> List('a', 'b', 'c', 'e', 'f', 'g'),
  1 -> List('c', 'f'),
  2 -> List('a', 'c', 'd', 'e', 'g'),
  3 -> List('a', 'c', 'd', 'f', 'g'),
  4 -> List('b', 'c', 'd', 'f'),
  5 -> List('a', 'b', 'd', 'f', 'g'),
  6 -> List('a', 'b', 'd', 'e', 'f', 'g'),
  7 -> List('a', 'c', 'f'),
  8 -> List('a', 'b', 'c', 'd', 'e', 'f', 'g'),
  9 -> List('a', 'b', 'c', 'd', 'f', 'g')
)

// Once findMapping has decided ot map the first sequence to a particular digit,
// this helper function dives into the first sequence and the actual segments that
// that are supposed to appear in that digit. It tries to match the sequence up to the expected
// things: if an element of the first sequence has already been assigned, it may give us a contradiction
// (stop and return None), or it may be consistent with the expected segments, in which case we
// move on to the next segment in the current sequence. If that input segment is unassigned,
// we make recursive calls to this function itself to try and assign and delve further into the decision
// tree.
// if *all* the input segments are assigned and there aren't any contradictions,
// we can move on to the next input sequence by calling back up to findMapping.
def findMappingWithDigit(
    currentMapping: Map[Char, Char],
    remainingSequences: Seq[String],
    tryingFirstDigitAs: Int
): Option[Map[Char, Char]] =
  if remainingSequences.isEmpty then return Some(currentMapping)
  val digit = remainingSequences.head
  val chars = digit.toList
  val expectedChars = digitToExpected(tryingFirstDigitAs)
  for char <- chars do
    currentMapping.get(char).map(expectedChars.contains(_)) match
      case Some(true) => {} // go to next char
      case Some(false) =>
        return None // incompatible matching is already set
      case None => {
        // try to assign char to each of the not-yet-assigned expected chars
        for
          charToAttempt <- expectedChars
          if currentMapping.values.find(_ == charToAttempt).isEmpty
        do
          val attempt = findMappingWithDigit(
            currentMapping.updated(char, charToAttempt),
            remainingSequences,
            tryingFirstDigitAs
          )
          if !attempt.isEmpty then
            // println(s"matched ${char} to ${charToAttempt}")
            return attempt
        // we tried assigning char to each one and none worked
        return None
      }
  // all chars matched
  return findMapping(currentMapping, remainingSequences.tail)

def segmentsToDigit(mapping: Map[Char, Char], segments: String): Int =
  val sortedSegments = segments.map(mapping(_)).toList.sorted
  for (k, v) <- digitToExpected do if sortedSegments == v then return k
  println(segments)
  println(sortedSegments)
  ???
