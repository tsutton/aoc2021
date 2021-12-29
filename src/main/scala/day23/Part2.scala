package day23

import scala.collection.mutable.{Map => MMap}

final val roomSize = 4

def amphidNumberToChar(i: Int): Char = i match
  case 0 => 'A'
  case 1 => 'B'
  case 2 => 'C'
  case 3 => 'D'
  case _ => ???

// amphipods 0 and 1 are A, 2 and 3 are B, 4 and 5 are C, 6 and 7 are D
case class BurrowPt2(val amphipods: List[PositionPt2]):
  override def toString: String =
    val buf = StringBuilder("#############\n#")
    for i <- 0 to 10 do
      lookup(PositionPt2.Hallway(i)) match
        case Some(j) => buf.addAll(amphidNumberToChar(j / roomSize).toString)
        case None    => buf.addOne('.')
    buf.addAll("#\n")
    for i <- (roomSize - 1) to 0 by -1 do
      if i + 1 == roomSize then buf.addAll("###") else buf.addAll("  #")
      for room <- 0 to 3 do
        buf.addOne(
          lookup(PositionPt2.Room(room, i))
            .map(amphid => amphidNumberToChar(amphid / roomSize))
            .getOrElse('.')
        )
        buf.addOne('#')
      if i + 1 == roomSize then buf.addAll("##\n") else buf.addAll("  \n")
    buf.toString

  def countAmphipodsInRoom(room: Int): Int =
    (0 until roomSize)
      .find(slot => lookup(PositionPt2.Room(room, slot)).isEmpty)
      .getOrElse(roomSize)

  // if the room has a wrong amphipod in it, returns Left[slot], slot lowest that's wrong
  // if the slot has only correct amphipods (or is empty) returns Right[slot], slot lowest that's open
  def roomSolvedUpTo(room: Int): Either[Int, Int] =
    for slot <- 0 until roomSize do
      lookup(PositionPt2.Room(room, slot)) match
        case Some(i) if i / roomSize != room => return Left(slot)
        case None                            => return Right(slot)
        case _                               => {}
    return Right(roomSize)

  def availableMovesForAmphipod(amphipod: Int): List[PositionPt2] = amphipods(
    amphipod
  ) match
    case PositionPt2.Room(roomNumber, slot) => {
      if slot + 1 < countAmphipodsInRoom(roomNumber) then return List()

      val targetRoom = amphipod / roomSize
      val targetRoomHallway = 2 * targetRoom + 2

      // check if already solved
      if roomNumber == targetRoom then
        roomSolvedUpTo(roomNumber) match
          case Right(j) if j >= slot => return List()
          case _                     => {}

      val ourHallwayPosition = roomNumber * 2 + 2
      val (minHallwayPos, maxHallwayPos) = availableHallwayRange(
        ourHallwayPosition
      )
      // all hallway positions from min to max are available. In addition, if our room is available, we must go to it
      if targetRoomHallway >= minHallwayPos && targetRoomHallway <= maxHallwayPos then
        // println(s"inner with ${roomSolvedUpTo(targetRoom)}")
        roomSolvedUpTo(targetRoom) match
          case Right(i) => return List(PositionPt2.Room(targetRoom, i))
          case Left(i)  => {}

      (minHallwayPos to maxHallwayPos)
        .filter(!List(2, 4, 6, 8).contains(_))
        .map(PositionPt2.Hallway(_))
        .toList
    }
    case PositionPt2.Hallway(slot) => {
      val targetRoom = amphipod / roomSize
      val targetRoomHallway = 2 * targetRoom + 2
      val (minHallwayPos, maxHallwayPos) = availableHallwayRange(slot)

      // if our room is ready, we go to it, otherwise we stay put
      if targetRoomHallway >= minHallwayPos && targetRoomHallway <= maxHallwayPos then
        // println(s"inner with ${roomSolvedUpTo(targetRoom)}")
        roomSolvedUpTo(targetRoom) match
          case Right(i) => return List(PositionPt2.Room(targetRoom, i))
          case Left(i)  => {}

      List()
    }

  def availableHallwayRange(current: Int): (Int, Int) =
    val occupiedHallwayPositions = amphipods
      .map(_ match
        case _: PositionPt2.Room    => None
        case PositionPt2.Hallway(i) => Some(i)
      )
      .filter(!_.isEmpty)
      .map(_.get)
    val minHallwayPos = occupiedHallwayPositions
      .filter(_ < current)
      .maxOption
      .map(_ + 1)
      .getOrElse(0)
    val maxHallwayPos = occupiedHallwayPositions
      .filter(_ > current)
      .minOption
      .map(_ - 1)
      .getOrElse(10)
    (minHallwayPos, maxHallwayPos)

  def lookup(pos: PositionPt2): Option[Int] =
    (0 until amphipods.size).find(amphipods(_) == pos)

  def isSolved: Boolean = amphipods.zipWithIndex
    .map((pos, i) =>
      pos match
        case PositionPt2.Room(room, _) if room == i / roomSize => true
        case _                                                 => false
    )
    .reduce(_ && _)

  def minimumEnergyToSolve(): Option[Int] =
    (0 until amphipods.size)
      .map(minimumEnergyToSolveWithFirstMover(_))
      .collect({ case Some(i) => i })
      .minOption

  def minimumEnergyToSolveWithFirstMover(amphipodToMove: Int): Option[Int] =
    if BurrowPt2.cache.contains((this, amphipodToMove)) then
      return BurrowPt2.cache((this, amphipodToMove))
    if isSolved then return Some(0)
    val moves = availableMovesForAmphipod(amphipodToMove)
    val possibles: List[Option[Int]] = for
      move <- moves
      nextAmphipod <- (0 until amphipods.size)
      if nextAmphipod != amphipodToMove
    yield
      val energyForThisMove: Int = amphipods(amphipodToMove)
        .distanceTo(move) * math.pow(10, amphipodToMove / roomSize).intValue
      withMove(amphipodToMove, move)
        .minimumEnergyToSolveWithFirstMover(nextAmphipod)
        .map(_ + energyForThisMove)
    val answer = possibles
      .collect({ case Some(i) =>
        i
      })
      .minOption
    BurrowPt2.cache((this, amphipodToMove)) = answer
    answer

  def withMove(amphipod: Int, position: PositionPt2): BurrowPt2 =
    val ret = BurrowPt2(amphipods.updated(amphipod, position))
    // println("moved to:\n")
    // println(ret.toString)
    ret

object BurrowPt2:
  val cache: MMap[(BurrowPt2, Int), Option[Int]] = MMap()

enum PositionPt2:
  def distanceTo(other: PositionPt2): Int = (this, other) match
    case (Hallway(thisSlot), Hallway(otherSlot)) => otherSlot - thisSlot
    case (Room(roomNumber, slot), Hallway(otherSlot)) => {
      val distanceToHallway = roomSize - slot
      distanceToHallway + (2 * roomNumber + 2 - otherSlot).abs
    }
    case (Hallway(_), Room(_, _)) => other.distanceTo(this)
    case (Room(thisRoomNumber, thisSlot), Room(otherRoomNumber, otherSlot)) => {
      val distanceToHallway = roomSize - thisSlot
      distanceToHallway + other.distanceTo(Hallway(2 * thisRoomNumber + 2))
    }

  // Room is 0 to 3, slot is 0 (inner, away from hallway) or 1 (outer, towards hallway)
  case Room(val roomNumber: Int, val slot: Int)
  // Slot is 0 to 10 representing the hallway, Slots 3, 5, 7, 9 are connected to the rooms
  case Hallway(val slot: Int)

// def exampleBurrowPt2: BurrowPt2 = BurrowPt2(
//   List(
//     // A
//     PositionPt2.Room(0, 0),
//     PositionPt2.Room(3, 0),
//     // B
//     PositionPt2.Room(0, 1),
//     PositionPt2.Room(2, 1),
//     // C
//     PositionPt2.Room(1, 1),
//     PositionPt2.Room(2, 0),
//     // D
//     PositionPt2.Room(1, 0),
//     PositionPt2.Room(3, 1)
//   )
// )

// def inputPt2: BurrowPt2 = BurrowPt2(
//   List(
//     PositionPt2.Room(0, 1),
//     PositionPt2.Room(3, 1),
//     PositionPt2.Room(2, 0),
//     PositionPt2.Room(2, 1),
//     PositionPt2.Room(1, 1),
//     PositionPt2.Room(3, 0),
//     PositionPt2.Room(0, 0),
//     PositionPt2.Room(1, 0)
//   )
// )

def exampleBurrowPt2: BurrowPt2 = BurrowPt2(
  List(
    // A
    PositionPt2.Room(0, 0),
    PositionPt2.Room(2, 1),
    PositionPt2.Room(3, 0),
    PositionPt2.Room(3, 2),
    // B
    PositionPt2.Room(0, 3),
    PositionPt2.Room(1, 1),
    PositionPt2.Room(2, 2),
    PositionPt2.Room(2, 3),
    // C
    PositionPt2.Room(1, 2),
    PositionPt2.Room(1, 3),
    PositionPt2.Room(2, 0),
    PositionPt2.Room(3, 1),
    // D
    PositionPt2.Room(0, 1),
    PositionPt2.Room(0, 2),
    PositionPt2.Room(1, 0),
    PositionPt2.Room(3, 3)
  )
)

def inputPt2: BurrowPt2 = BurrowPt2(
  List(
    // A
    PositionPt2.Room(0, 3),
    PositionPt2.Room(3, 3),
    PositionPt2.Room(2, 1),
    PositionPt2.Room(3, 2),
    // B
    PositionPt2.Room(2, 0),
    PositionPt2.Room(1, 1),
    PositionPt2.Room(2, 2),
    PositionPt2.Room(2, 3),
    // C
    PositionPt2.Room(1, 3),
    PositionPt2.Room(3, 0),
    PositionPt2.Room(1, 2),
    PositionPt2.Room(3, 1),
    // D
    PositionPt2.Room(0, 0),
    PositionPt2.Room(1, 0),
    PositionPt2.Room(0, 1),
    PositionPt2.Room(0, 2)
  )
)

def part2(): Int =
  val burrow = inputPt2
  println(burrow)
  assert(burrow.amphipods.toSet.size == roomSize * roomSize)
  burrow.minimumEnergyToSolve().get
