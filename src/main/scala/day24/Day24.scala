package day24

import scala.collection.mutable.ArrayBuffer
import day24.Register

/* a block in the input

   the input can be divided into 14 blocks of 18 lines, such that each block looks like this:

   inp w
   mul x 0
   add x z
   mod x 26
   div z LINE5
   add x LINE6
   eql x w
   eql x 0
   mul y 0
   add y 25
   mul y x
   add y 1
   mul z y
   mul y 0
   add y w
   add y LINE16
   mul y x
   add z y

 x and y are zeroed before use. Therefore a block computes a new z as a function of (z, w)

 */
case class Block(val line5: Int, val line6: Int, val line16: Int):
  def compute(zInput: Long, wInput: Long): Long =
    var x: Long = zInput % 26 + line6
    var z: Long = zInput / line5
    x = if x == wInput then 0 else 1
    z *= (25 * x + 1)
    z += (wInput + line16) * x
    z
// if we get the else case, then we get z = (z / line5) * 26 + (w + line16)
// if we get the true case, we get z = (z / line5)
// all of the line5==1 cases have line6 > 10, so we actually CAN'T get the true case for them.
// so:
// z0: i(0)+6
// z1: 26*z0 + i(1) + 14
// z2: 26*z1 + i(2) + 14
// now we come to input(3). To get 0, we *must* get the true condition for all 26s. So:
//   i(2)+14 - 8 = i(3).
// z3= z1
// z4= 26*z1 + i(4)+9
// z5= 26*z4 + i(5)+12
//   i(5) + 12 - 11 = i(6)
// z6= z4 = 26*z1 + i(4)+9
//   i(4) + 9 - 4 = i(7)
// z7= z1 = 26*z(0) + i(1)+14
//   i(1) + 14 -15 = i(8)
// z8= z0 = i(0) + 6
// z9=26 * z8 + i(9)+6
// z10=26*z9 + i(10)+9
//    i(10)+9-1 = i(11)
// z11=z9=26 * z8 + i(9)+6
//    i(9)+6-8=i(12)
// z12=z8=i(0)+6
//    i(0)+6-14=i(13)
// z13=0

// conditions:
// i3=i2+6
// i6=i5+1
// i7=i4+5
// i8=i2-1
// i11=i10+8
// i12=i9-2
// i13=i0-8
// (i0, i1, i2, i2+6, i4, i5, i5+1, i4+5,i1-1, i9, i10, i10+8,i9-2,i0-8)
// i0 = 9, i1 <- (1,9), i2 <- (2, 3), i4 <- (1,4)
val input = List(
  Block(1, 11, 6), // 0
  Block(1, 13, 14), // 1
  Block(1, 15, 14), // 2
  Block(26, -8, 10), // 3
  Block(1, 13, 9), // 4
  Block(1, 15, 12), // 5
  Block(26, -11, 8), // 6
  Block(26, -4, 13), // 7
  Block(26, -15, 12), // 8
  Block(1, 14, 6), // 9
  Block(1, 14, 9), // 10
  Block(26, -1, 15), // 11
  Block(26, -8, 4), // 12
  Block(26, -14, 10) // 13
)

def computeAll(l: List[Long]): Long =
  input.zip(l).foldLeft(0L)((z, pair) => pair._1.compute(z, pair._2))

// While it wasn't necesarry, I made a normal interpreter for this ALU to verify my logic

enum Register:
  def toIndex: Int = this match
    case X => 0
    case Y => 1
    case Z => 2
    case W => 3

  case X, Y, Z, W

object Register:
  def fromIndex(i: Int): Register = i match
    case 0 => X
    case 1 => Y
    case 2 => Z
    case 3 => W
    case _ => ???

  def fromString(s: String): Register = s match
    case "x" => X
    case "y" => Y
    case "z" => Z
    case "w" => W
    case _   => ???

enum Op:
  case Add, Mul, Div, Mod, Inp, Eql

case class Instruction(
    val op: Op,
    val target: Register,
    val source: Register | Long | Null
)

object Instruction:
  def fromString(s: String): Instruction =
    val parts = s.split(' ')
    val op = parts(0) match
      case "inp" => Op.Inp
      case "add" => Op.Add
      case "mul" => Op.Mul
      case "div" => Op.Div
      case "mod" => Op.Mod
      case "eql" => Op.Eql
      case _     => ???
    val target = Register.fromString(parts(1))
    val source: Register | Long | Null =
      if op == Op.Inp then null
      else if List("x", "y", "z", "w").contains(parts(2)) then
        Register.fromString(parts(2))
      else Integer.parseInt(parts(2)).toLong
    Instruction(op, target, source)

class ALU(val registers: ArrayBuffer[Long] = ArrayBuffer(0, 0, 0, 0)):
  def evaluate(instr: Instruction, input: Long | Null = null): Boolean =
    instr.op match
      case Op.Inp => {
        registers(instr.target.toIndex) = input match
          case null    => ???
          case i: Long => i
        true
      }
      case Op.Add => {
        registers(instr.target.toIndex) += get(instr.source)
        false
      }
      case Op.Mul => {
        registers(instr.target.toIndex) *= get(instr.source)
        false
      }
      case Op.Div => {
        registers(instr.target.toIndex) /= get(instr.source)
        false
      }
      case Op.Mod => {
        registers(instr.target.toIndex) %= get(instr.source)
        false
      }
      case Op.Eql => {
        registers(instr.target.toIndex) =
          if get(instr.target) == get(instr.source) then 1 else 0
        false
      }

  def get(source: Register | Long | Null): Long = source match
    case null        => ???
    case i: Long     => i
    case r: Register => registers(r.toIndex)

def runProgramString(programStr: String, input: List[Long]): Long =
  val program = programStr.linesIterator.map(Instruction.fromString)
  val alu = ALU()
  var inputIdx = 0
  for instr <- program do
    val in = if instr.op == Op.Inp then
      inputIdx += 1
      input(inputIdx - 1)
    else null
    alu.evaluate(instr, in)
  alu.registers(2)

val testProgram1 = """inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2
"""

val wholeInput = """inp w
mul x 0
add x z
mod x 26
div z 1
add x 11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 6
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 14
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 14
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -8
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 9
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 12
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -4
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 12
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 6
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 9
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -1
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 15
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -8
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 4
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y
"""
