package day10

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable

def input: Iterator[String] =
  Source.fromResource("day10.txt").getLines

def exampleInput: Iterator[String] = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
""".linesIterator

def part1(): Int =
  val scores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )
  input
    .map(autocomplete(_))
    .filter(_.isLeft)
    .map((c: Either[Char, String]) => scores(c.left.getOrElse(???)))
    .sum

def part2(): Long =
  val scores =
    input
      .map(autocomplete(_))
      .filter(_.isRight)
      .map(_.getOrElse(???))
      // .map(i => { println(i); i })
      .map(autocompleteScore)
      .toVector
      .sortBy(i => i)
  // println(scores)
  scores((scores.size - 1) / 2)

// If we run into an unexpected char (corrupted line as in part 1),
// returns Left(that char)
// Or else we return the Right(autocompletion)
def autocomplete(line: String): Either[Char, String] =
  val stack = Stack[Char]()
  val closeToOpen = Map(
    ')' -> '(',
    '>' -> '<',
    '}' -> '{',
    ']' -> '['
  )
  val openToClose = Map(
    '(' -> ')',
    '<' -> '>',
    '{' -> '}',
    '[' -> ']'
  )
  for c <- line do
    c match
      case '(' | '[' | '{' | '<' => stack.push(c)
      case ')' | ']' | '}' | '>' => {
        val last = stack.pop()
        if last != closeToOpen(c) then return Left(c)
      }
  val s = mutable.StringBuilder()
  while !stack.isEmpty do s += openToClose(stack.pop())
  Right(s.toString)

def autocompleteScore(s: String): Long =
  val scores = Map[Char, Long](
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )
  s.foldLeft(0L)((acc, char) => acc * 5L + scores(char))
