package test

import utest.*
import day23.*

object Day23Test extends TestSuite:
  val tests = Tests {
    test("lookup") {
      assert(exampleBurrow.lookup(Position.Room(0, 0)).map(_ / 2) == Some(0))
      assert(exampleBurrow.lookup(Position.Room(0, 1)).map(_ / 2) == Some(1))
      assert(exampleBurrow.lookup(Position.Room(1, 0)).map(_ / 2) == Some(3))
      assert(exampleBurrow.lookup(Position.Room(1, 1)).map(_ / 2) == Some(2))
      assert(exampleBurrow.lookup(Position.Room(2, 0)).map(_ / 2) == Some(2))
      assert(exampleBurrow.lookup(Position.Room(2, 1)).map(_ / 2) == Some(1))
      assert(exampleBurrow.lookup(Position.Room(3, 0)).map(_ / 2) == Some(0))
      assert(exampleBurrow.lookup(Position.Room(3, 1)).map(_ / 2) == Some(3))
      for i <- 0 to 10 do
        assert(exampleBurrow.lookup(Position.Hallway(i)).isEmpty)
    }
    test("available from start") {
      assert(exampleBurrow.availableMovesForAmphipod(0).isEmpty)
      val movesFor2 = exampleBurrow.availableMovesForAmphipod(2)
      assert(
        movesFor2 == List(
          Position.Hallway(0),
          Position.Hallway(1),
          Position.Hallway(3),
          Position.Hallway(5),
          Position.Hallway(7),
          Position.Hallway(9),
          Position.Hallway(10)
        )
      )
      var burrow = exampleBurrow.withMove(3, Position.Hallway(3))
      // it's in the correct place already
      assert(burrow.availableMovesForAmphipod(0).isEmpty)
      // its room isn't available
      assert(burrow.availableMovesForAmphipod(3).isEmpty)
      var moves = burrow.availableMovesForAmphipod(7)
      assert(
        moves == List(
          Position.Hallway(5),
          Position.Hallway(7),
          Position.Hallway(9),
          Position.Hallway(10)
        )
      )
      moves = burrow.availableMovesForAmphipod(4)
      assert(
        moves == List(
          Position.Room(2, 1)
        )
      )
      moves = burrow.availableMovesForAmphipod(5)
      assert(
        moves == List()
      )
    }
    test("isSolved") {
      assert(!exampleBurrow.isSolved)

    }
  }

object Day23Pt2Test extends TestSuite:
  val tests = Tests {
    test("lookup") {
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(0, 0)).map(_ / 2) == Some(0)
      )
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(0, 1)).map(_ / 2) == Some(1)
      )
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(1, 0)).map(_ / 2) == Some(3)
      )
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(1, 1)).map(_ / 2) == Some(2)
      )
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(2, 0)).map(_ / 2) == Some(2)
      )
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(2, 1)).map(_ / 2) == Some(1)
      )
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(3, 0)).map(_ / 2) == Some(0)
      )
      assert(
        exampleBurrowPt2.lookup(PositionPt2.Room(3, 1)).map(_ / 2) == Some(3)
      )
      for i <- 0 to 10 do
        assert(exampleBurrowPt2.lookup(PositionPt2.Hallway(i)).isEmpty)
    }
    test("available from start") {
      var moves = exampleBurrowPt2.availableMovesForAmphipod(0)
      assert(moves.isEmpty)
      val movesFor2 = exampleBurrowPt2.availableMovesForAmphipod(2)
      assert(
        movesFor2 == List(
          PositionPt2.Hallway(0),
          PositionPt2.Hallway(1),
          PositionPt2.Hallway(3),
          PositionPt2.Hallway(5),
          PositionPt2.Hallway(7),
          PositionPt2.Hallway(9),
          PositionPt2.Hallway(10)
        )
      )
      var burrow = exampleBurrowPt2.withMove(3, PositionPt2.Hallway(3))
      // it's in the correct place already
      assert(burrow.availableMovesForAmphipod(0).isEmpty)
      // its room isn't available
      assert(burrow.availableMovesForAmphipod(3).isEmpty)
      moves = burrow.availableMovesForAmphipod(7)
      assert(
        moves == List(
          PositionPt2.Hallway(5),
          PositionPt2.Hallway(7),
          PositionPt2.Hallway(9),
          PositionPt2.Hallway(10)
        )
      )
      var solvedUpTo = burrow.roomSolvedUpTo(2)
      assert(solvedUpTo == Right(1))
      moves = burrow.availableMovesForAmphipod(4)
      assert(
        moves == List(
          PositionPt2.Room(2, 1)
        )
      )
      moves = burrow.availableMovesForAmphipod(5)
      assert(
        moves == List()
      )
      moves = burrow.availableMovesForAmphipod(3)
      assert(
        moves == List()
      )

      burrow = burrow.withMove(4, PositionPt2.Room(2, 1))
      moves = burrow.availableMovesForAmphipod(3)
      assert(
        moves == List()
      )

      burrow = burrow.withMove(6, PositionPt2.Hallway(10))
      moves = burrow.availableMovesForAmphipod(3)
      assert(
        moves == List(PositionPt2.Room(1, 0))
      )
    }
    test("isSolved false") {
      assert(!exampleBurrowPt2.isSolved)
    }
    test("isSolved true") {
      val solvedBurrow = BurrowPt2(
        List(
          PositionPt2.Room(0, 0),
          PositionPt2.Room(0, 1),
          PositionPt2.Room(1, 0),
          PositionPt2.Room(1, 1),
          PositionPt2.Room(2, 0),
          PositionPt2.Room(2, 1),
          PositionPt2.Room(3, 0),
          PositionPt2.Room(3, 1)
        )
      )
      assert(
        solvedBurrow.isSolved
      )
      assert(
        BurrowPt2(
          List(
            PositionPt2.Room(0, 1),
            PositionPt2.Room(0, 0),
            PositionPt2.Room(1, 1),
            PositionPt2.Room(1, 0),
            PositionPt2.Room(2, 0),
            PositionPt2.Room(2, 1),
            PositionPt2.Room(3, 0),
            PositionPt2.Room(3, 1)
          )
        ).isSolved
      )
      assert(solvedBurrow.availableMovesForAmphipod(0).isEmpty)
      var moves = solvedBurrow.availableMovesForAmphipod(1)
      assert(moves.isEmpty)
      for i <- 0 to 7 do
        assert(solvedBurrow.availableMovesForAmphipod(i).isEmpty)
    }
    // test("can't move out if already there") {}
  }
