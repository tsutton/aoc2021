package test

import utest.*
import day19.{Reorientation, Point, Grid, parse, tryMatch}
import scala.collection.mutable.{Set => MSet}

object ReorientationTest extends TestSuite:
  val tests = Tests {
    test("identity reorientation") {
      val ori = Reorientation(Point(1, 0, 0), Point(0, 1, 0))
      assert(ori.imageOfZ == Point(0, 0, 1))
      val somePoints = List(
        Point(1, 0, 0),
        Point(0, 1, 0),
        Point(0, 0, 1),
        Point(1, 2, 3),
        Point(0, 0, 0)
      )
      for p <- somePoints do assert(ori.reorientPoint(p) == p)
    }
    test("rotate 90 on x") {
      val ori = Reorientation(Point(1, 0, 0), Point(0, 0, 1))
      assert(ori.imageOfZ == Point(0, -1, 0))
      val somePoints = List(
        Point(1, 0, 0),
        Point(0, 1, 0),
        Point(0, 0, 1),
        Point(1, 2, 3),
        Point(0, 0, 0)
      )
      val expectedPoints = List(
        Point(1, 0, 0),
        Point(0, 0, 1),
        Point(0, -1, 0),
        Point(1, -3, 2),
        Point(0, 0, 0)
      )
      for (p, q) <- somePoints.zip(expectedPoints) do
        assert(ori.reorientPoint(p) == q)
    }
    test("axes under all rotations") {
      val axes = List(Point(1, 0, 0), Point(0, 1, 0), Point(0, 0, 1))
      val allExpected = Set(
        (1, 0, 0),
        (-1, 0, 0),
        (0, 1, 0),
        (0, -1, 0),
        (0, 0, 1),
        (0, 0, -1)
      ).map(Point(_))
      for p <- axes do
        val allFound = Reorientation
          .allReorienations()
          .map(_.reorientPoint(p))
          .toSet
        assert(allFound.size == 6)
        assert(
          allFound == allExpected
        )
    }
    test("one point under all rotations") {
      val p = Point(1, 2, 3)
      val allPossible = Set(
        (1, 2, 3),
        (2, 3, 1),
        (3, 1, 2),
        (1, -3, 2),
        (-3, 2, 1),
        (-2, 1, 3),
        (1, 3, -2),
        (3, 2, -1),
        (2, -1, 3),
        (-1, 3, 2),
        (3, -2, 1),
        (2, 1, -3),
        (1, -2, -3),
        (-1, 2, -3),
        (-1, -2, 3),
        (-2, -3, 1),
        (-3, 1, -2),
        (2, -3, -1),
        (-3, -1, 2),
        (-2, 3, -1),
        (3, -1, -2),
        (-1, -3, -2),
        (-3, -2, -1),
        (-2, -1, -3)
      ).map(Point(_))
      assert(allPossible.size == 24)
      val allFound = Reorientation
        .allReorienations()
        .map(_.reorientPoint(p))
        .toSet
      assert(allFound.size == 24)
      // println(allFound -- allPossible)
      assert(
        allFound == allPossible
      )
    }
    test("grid in orientation") {
      val p = Point(1, 2, 3)
      val allPossible = Set(
        (1, 2, 3),
        (2, 3, 1),
        (3, 1, 2),
        (1, -3, 2),
        (-3, 2, 1),
        (-2, 1, 3),
        (1, 3, -2),
        (3, 2, -1),
        (2, -1, 3),
        (-1, 3, 2),
        (3, -2, 1),
        (2, 1, -3),
        (1, -2, -3),
        (-1, 2, -3),
        (-1, -2, 3),
        (-2, -3, 1),
        (-3, 1, -2),
        (2, -3, -1),
        (-3, -1, 2),
        (-2, 3, -1),
        (3, -1, -2),
        (-1, -3, -2),
        (-3, -2, -1),
        (-2, -1, -3)
      ).map(Point(_))
      assert(allPossible.size == 24)
      val foundPoints = MSet[Point]()
      for
        orientation <- Reorientation
          .allReorienations()
      do
        var grid = Grid()
        grid.addPoint(p.toTuple)
        grid = grid.inOrientation(orientation)
        assert(grid.points.size == 1)
        foundPoints.add(grid.points(0))
      // println(foundPoints)
      assert(foundPoints.size == 24)
    }
    test("grid shift") {
      val grid = Grid()
      grid.addPoint((0, 0, 0))
      for
        shift <- List(
          (1, 0, 0),
          (0, 1, 0),
          (0, 0, 1),
          (1, 2, 3)
        )
      do
        val newGrid = grid.shift(shift)
        assert(newGrid.points.size == 1)
        assert(newGrid.points(0) == Point(shift))
    }
    test("match with shift only") {
      val grid = Grid()
      for i <- (1 to 12) do grid.addPoint((i, i, i))
      for
        shift <- List(
          (1, 0, 0),
          (0, 1, 0),
          (0, 0, 1),
          (1, 2, 3)
        )
      do
        val newGrid = grid.shift(shift)
        for j <- (13 to 24) do
          grid.addPoint((j, j + 1, j + 2))
          newGrid.addPoint(2 * j, 3 * j, 4 * j)
        assert(newGrid.matches(grid) == Some(shift))
        assert(
          grid.matches(newGrid) == Some((-shift._1, -shift._2, -shift._3))
        )
    }
    test("match with orientation only") {
      val grid = Grid()
      for i <- (1 to 12) do grid.addPoint((i, i, i))
      for orientation <- Reorientation.allReorienations().slice(0, 2) do
        val newGrid = grid.inOrientation(orientation)
        for j <- (13 to 24) do
          grid.addPoint((j, j + 1, j + 2))
          newGrid.addPoint(2 * j, 3 * j, 4 * j)
        assert(
          newGrid.matches(grid.inOrientation(orientation)) == Some((0, 0, 0))
        )
        assert(
          grid.inOrientation(orientation).matches(newGrid) == Some((0, 0, 0))
        )
    }
    test("match with orientation and shift") {
      val grid = Grid()
      for i <- (1 to 12) do grid.addPoint((i, i, i))
      val shift = (1, 2, 3)
      for orientation <- Reorientation.allReorienations().slice(0, 2) do
        var newGrid = grid.inOrientation(orientation)
        for j <- (13 to 24) do
          grid.addPoint((j, j + 1, j + 2))
          newGrid.addPoint(2 * j, 3 * j, 4 * j)
        newGrid = newGrid.shift(shift)
        assert(
          newGrid.matches(grid.inOrientation(orientation)) == Some(shift)
        )
        assert(
          grid.inOrientation(orientation).matches(newGrid) == Some((-1, -2, -3))
        )
    }
    test("example_from_problem") {
      val i = """--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14

"""
      val grids = parse(i.linesIterator)
      assert(grids.size == 5)
      var matc = tryMatch(grids(0), grids(1))
      assert(!matc.isEmpty)

      matc = tryMatch(grids(1), grids(4))
      println(matc)
      assert(!matc.isEmpty)
    }
  }
