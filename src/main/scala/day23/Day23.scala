package day23

import scala.collection.mutable.{Map => MMap}
// amphipods 0 and 1 are A, 2 and 3 are B, 4 and 5 are C, 6 and 7 are D
case class Burrow(val amphipods: List[Position]):

  def availableMovesForAmphipod(amphipod: Int): List[Position] = amphipods(
    amphipod
  ) match
    case Position.Room(roomNumber, slot) => {
      if slot == 0 && lookup(Position.Room(roomNumber, 1)).isDefined then
        return List()

      val targetRoom = amphipod / 2
      val targetRoomHallway = 2 * targetRoom + 2

      // check if already solved
      if roomNumber == targetRoom && slot == 0 then return List()
      else if roomNumber == targetRoom && lookup(Position.Room(roomNumber, 0))
          .map(_ / 2) == Some(roomNumber)
      then return List()

      val ourHallwayPosition = roomNumber * 2 + 2
      val (minHallwayPos, maxHallwayPos) = availableHallwayRange(
        ourHallwayPosition
      )
      // all hallway positions from min to max are available. In addition, if our room is available, we must go to it
      if targetRoomHallway >= minHallwayPos && targetRoomHallway <= maxHallwayPos then
        if lookup(Position.Room(targetRoom, 0)).isEmpty then
          return List(Position.Room(targetRoom, 0))
        else if (
          lookup(Position.Room(targetRoom, 0))
            .map(_ / 2 == targetRoom)
            .getOrElse(false)
          &&
          lookup(Position.Room(targetRoom, 1)).isEmpty
        ) then return List(Position.Room(targetRoom, 1))

      (minHallwayPos to maxHallwayPos)
        .filter(!List(2, 4, 6, 8).contains(_))
        .map(Position.Hallway(_))
        .toList
    }
    case Position.Hallway(slot) => {
      val targetRoom = amphipod / 2
      val targetRoomHallway = 2 * targetRoom + 2
      val (minHallwayPos, maxHallwayPos) = availableHallwayRange(slot)

      // if our room is ready, we go to it, otherwise we stay put
      if targetRoomHallway >= minHallwayPos && targetRoomHallway <= maxHallwayPos then
        if lookup(Position.Room(targetRoom, 0)).isEmpty then
          return List(Position.Room(targetRoom, 0))
        else if (
          lookup(Position.Room(targetRoom, 0))
            .map(_ / 2 == targetRoom)
            .getOrElse(false)
          &&
          lookup(Position.Room(targetRoom, 1)).isEmpty
        ) then return List(Position.Room(targetRoom, 1))

      List()
    }

  def availableHallwayRange(current: Int): (Int, Int) =
    val occupiedHallwayPositions = amphipods
      .map(_ match
        case _: Position.Room    => None
        case Position.Hallway(i) => Some(i)
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

  def lookup(pos: Position): Option[Int] =
    (0 until amphipods.size).find(amphipods(_) == pos)

  def isSolved: Boolean = amphipods.zipWithIndex
    .map((pos, i) =>
      pos match
        case Position.Room(room, _) if room == i / 2 => true
        case _                                       => false
    )
    .reduce(_ && _)

  def minimumEnergyToSolve(): Option[Int] =
    (0 until amphipods.size)
      .map(minimumEnergyToSolveWithFirstMover(_))
      .collect({ case Some(i) => i })
      .minOption

  def minimumEnergyToSolveWithFirstMover(amphipodToMove: Int): Option[Int] =
    if Burrow.cache.contains((this, amphipodToMove)) then
      return Burrow.cache((this, amphipodToMove))
    if isSolved then return Some(0)
    val moves = availableMovesForAmphipod(amphipodToMove)
    val possibles: List[Option[Int]] = for
      move <- moves
      nextAmphipod <- (0 until amphipods.size)
      if nextAmphipod != amphipodToMove
    yield
      val energyForThisMove: Int = amphipods(amphipodToMove)
        .distanceTo(move) * math.pow(10, amphipodToMove / 2).intValue
      withMove(amphipodToMove, move)
        .minimumEnergyToSolveWithFirstMover(nextAmphipod)
        .map(_ + energyForThisMove)
    val answer = possibles
      .collect({ case Some(i) =>
        i
      })
      .minOption
    Burrow.cache((this, amphipodToMove)) = answer
    answer

  def withMove(amphipod: Int, position: Position): Burrow =
    Burrow(amphipods.updated(amphipod, position))

object Burrow:
  val cache: MMap[(Burrow, Int), Option[Int]] = MMap()

enum Position:
  def distanceTo(other: Position): Int = (this, other) match
    case (Hallway(thisSlot), Hallway(otherSlot)) => otherSlot - thisSlot
    case (Room(roomNumber, slot), Hallway(otherSlot)) => {
      val distanceToHallway = if slot == 0 then 2 else 1
      distanceToHallway + (2 * roomNumber + 2 - otherSlot).abs
    }
    case (Hallway(_), Room(_, _)) => other.distanceTo(this)
    case (Room(thisRoomNumber, thisSlot), Room(otherRoomNumber, otherSlot)) => {
      val distanceToHallway = if thisSlot == 0 then 2 else 1
      distanceToHallway + other.distanceTo(Hallway(2 * thisRoomNumber + 2))
    }

  // Room is 0 to 3, slot is 0 (inner, away from hallway) or 1 (outer, towards hallway)
  case Room(val roomNumber: Int, val slot: Int)
  // Slot is 0 to 10 representing the hallway, Slots 3, 5, 7, 9 are connected to the rooms
  case Hallway(val slot: Int)

def exampleBurrow: Burrow = Burrow(
  List(
    Position.Room(0, 0),
    Position.Room(3, 0),
    Position.Room(0, 1),
    Position.Room(2, 1),
    Position.Room(1, 1),
    Position.Room(2, 0),
    Position.Room(1, 0),
    Position.Room(3, 1)
  )
)

def input: Burrow = Burrow(
  List(
    Position.Room(0, 1),
    Position.Room(3, 1),
    Position.Room(2, 0),
    Position.Room(2, 1),
    Position.Room(1, 1),
    Position.Room(3, 0),
    Position.Room(0, 0),
    Position.Room(1, 0)
  )
)

def part1(): Int =
  input.minimumEnergyToSolve().get
