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
