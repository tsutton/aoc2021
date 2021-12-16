package day16

import scala.io.Source
import scala.collection.mutable

def input: Iterator[String] =
  Source.fromResource("day16.txt").getLines

def hexToBits(hex: String): scala.collection.IndexedSeq[Char] =
  // for
  //   c <- hex.toIterable
  //   b <- Integer.parseInt(c.toString, 16).toBinaryString.pad
  // yield b
  hex.flatMap(_ match
    case '0' => "0000"
    case '1' => "0001"
    case '2' => "0010"
    case '3' => "0011"
    case '4' => "0100"
    case '5' => "0101"
    case '6' => "0110"
    case '7' => "0111"
    case '8' => "1000"
    case '9' => "1001"
    case 'A' => "1010"
    case 'B' => "1011"
    case 'C' => "1100"
    case 'D' => "1101"
    case 'E' => "1110"
    case 'F' => "1111"
  )

def readPacket(bits: Iterator[Char]): (Packet, Int) =
  val version = readVersion(bits)
  val packetType = readType(bits)
  // println(s"reading packet with v${version} and type ${packetType}")
  val (packet, len) = packetType match
    case 4   => readLiteral(version, bits)
    case typ => readOperator(version, typ, bits)
  println(packet)
  (packet, len + 6) // 3 for version, 3 for type

def takeFromBits(bits: Iterator[Char], length: Int): String =
  val chars = for _ <- 0 until length yield bits.next
  // println(chars.mkString)
  chars.mkString

def readVersion(bits: Iterator[Char]): Int =
  val verString = takeFromBits(bits, 3)
  Integer.parseInt(verString, 2)

def readType(bits: Iterator[Char]): Int =
  val verString = takeFromBits(bits, 3)
  Integer.parseInt(verString, 2)

def readLiteral(version: Int, bits: Iterator[Char]): (Packet.Literal, Int) =
  var continue = true
  val buf = StringBuilder()
  var bitsTaken = 0
  while continue do
    continue = bits.next match
      case '0' => false
      case _   => true
    buf.appendAll(takeFromBits(bits, 4))
    bitsTaken += 5
  (
    Packet.Literal(version, java.lang.Long.parseLong(buf.toString, 2)),
    bitsTaken
  )

def readOperator(
    version: Int,
    typ: Int,
    bits: Iterator[Char]
): (Packet.Operator, Int) =
  val (p, read) = bits.next match
    case '0' => readOperatorByLength(version, typ, bits)
    case _   => readOperatorByNumber(version, typ, bits)
  (p, read + 1)

def readOperatorByLength(
    version: Int,
    typ: Int,
    bits: Iterator[Char]
): (Packet.Operator, Int) =
  var subpackets = List[Packet]()
  val length = Integer.parseInt(takeFromBits(bits, 15), 2)
  // println(s"reading for length ${length}")
  var lengthRead = 0
  while lengthRead < length do
    val (packet, pLength) = readPacket(bits)
    // println(s"read subpacket of length ${pLength}")
    lengthRead += pLength
    subpackets = packet :: subpackets
  (
    Packet.Operator(version, subpackets.reverse, Operation.fromType(typ)),
    lengthRead + 15
  )

def readOperatorByNumber(
    version: Int,
    typ: Int,
    bits: Iterator[Char]
): (Packet.Operator, Int) =
  var subpackets = List[Packet]()
  val number = Integer.parseInt(takeFromBits(bits, 11), 2)
  var lengthRead = 0
  while subpackets.length < number do
    val (packet, pLength) = readPacket(bits)
    lengthRead += pLength
    subpackets = packet :: subpackets
  (
    Packet.Operator(version, subpackets.reverse, Operation.fromType(typ)),
    lengthRead + 11
  )

def part1(): Int =
  // val i = "A0016C880162017C3686B18A3D4780"
  // println(hexToBits(i).mkString)
  val i = input.next
  println(i)
  readPacket(hexToBits(i).iterator)._1.sumOfVersions

def part2(): Long =
  // val i = "F600BC2D8F"
  val i = input.next
  println(i)
  readPacket(hexToBits(i).iterator)._1.evaluate

enum Packet(val version: Int):
  def sumOfVersions: Int = this match
    case Literal(ver, _) => ver
    case o: Operator     => o.version + o.packets.map(_.sumOfVersions).sum

  def evaluate: Long = this match
    case Literal(ver, value) => value
    case o: Operator         => o.operation.evaluate(o.packets.map(_.evaluate))

  case Literal(override val version: Int, val value: Long)
      extends Packet(version)
  case Operator(
      override val version: Int,
      packets: List[Packet],
      operation: Operation
  ) extends Packet(version)

enum Operation:
  def evaluate(packets: List[Long]): Long = this match
    case Sum         => packets.sum
    case Product     => packets.product
    case Minimum     => packets.min
    case Maximum     => packets.max
    case GreaterThan => if packets(0) > packets(1) then 1 else 0
    case LessThan    => if packets(0) < packets(1) then 1 else 0
    case Equal       => if packets(0) == packets(1) then 1 else 0

  case Sum, Product, Minimum, Maximum, GreaterThan, LessThan, Equal

object Operation:
  def fromType(typ: Int): Operation = typ match
    case 0 => Sum
    case 1 => Product
    case 2 => Minimum
    case 3 => Maximum
    case 5 => GreaterThan
    case 6 => LessThan
    case 7 => Equal
