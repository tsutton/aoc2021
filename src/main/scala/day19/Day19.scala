package day19

import scala.collection.mutable.{ArrayBuffer, Set => MSet}

class Grid(val points: ArrayBuffer[Point] = ArrayBuffer[Point]()):

  def addPoint(point: (Int, Int, Int)): Unit =
    points.addOne(Point(point))

  /** The triple such that if you add this triple to all points of other, it
    * gives this, or None, if they don't match
    */
  def matches(other: Grid): Option[(Int, Int, Int)] =
    val referencePt = points.head
    for point <- other.points do
      val shift = referencePt - point
      val overlap = (other
        .shift(shift.toTuple)
        .points
        .toSet & points.toSet)
      assert(overlap.contains(referencePt))
      if overlap.size >= 12 then return Some(shift.toTuple)
    None

  def inOrientation(orientation: Reorientation): Grid =
    Grid(points.map(orientation.reorientPoint(_)))
  def shift(x: Int, y: Int, z: Int): Grid =
    Grid(points.map(pt => Point(pt.x + x, pt.y + y, pt.z + z)))
  def shift(p: (Int, Int, Int)): Grid = shift(p._1, p._2, p._3)

  override def toString(): String =
    s"""Grid(${points.mkString(", ")})"""

/** Represents a transformation of 3-d space
  *
  * In this reoriention, the point (1,0,0) maps to imageOfX, and similarly for
  * (0,1,0) The image of (0,0,1) is then determined by cross-product.
  */
case class Reorientation(
    val imageOfX: Point,
    val imageOfY: Point
):
  def imageOfZ: Point = Point(
    imageOfX.y * imageOfY.z - imageOfX.z * imageOfY.y,
    imageOfX.z * imageOfY.x - imageOfX.x * imageOfY.z,
    imageOfX.x * imageOfY.y - imageOfX.y * imageOfY.x
  )

  def reorientPoint(point: Point): Point =
    imageOfX * point.x + imageOfY * point.y + imageOfZ * point.z

object Reorientation:
  def allReorienations(): List[Reorientation] =
    val unitVectors = List(
      (1, 0, 0),
      (-1, 0, 0),
      (0, 1, 0),
      (0, -1, 0),
      (0, 0, 1),
      (0, 0, -1)
    ).map(Point(_))
    for
      x <- unitVectors
      y <- unitVectors
      if x != y && x != -y
    yield Reorientation(x, y)

case class Point(val x: Int, val y: Int, val z: Int):
  def +(other: Point) = Point(x + other.x, y + other.y, z + other.z)
  def -(other: Point) = Point(x - other.x, y - other.y, z - other.z)
  def unary_- = Point(-x, -y, -z)
  def *(by: Int) = Point(x * by, y * by, z * by)

  def toTuple = (x, y, z)

object Point:
  def apply(point: (Int, Int, Int)): Point = Point(point._1, point._2, point._3)

/*
 strategy: start with the first scanner. Find all scanners that match, and reorient + shift them
 those scanners are now locked, but unvisited. For each unvisited scanner, find matching ones, then
 shift and reorient. Etc.

 at the end, they are all shifted and oriented appropriately, so we just union their points.
 */

def exampleInput: Iterator[String] = """--- scanner 0 ---
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
""".linesIterator

def parse(input: Iterator[String]): ArrayBuffer[Grid] =
  val result = ArrayBuffer[Grid]()
  val buffered = input.buffered
  while buffered.hasNext do
    // we don't care about the --- scanner NNN --- line since they are in order
    buffered.next
    val grid = Grid()
    while buffered.hasNext && buffered.head != "" do
      val parts = buffered.next.split(',').map(Integer.parseInt(_))
      grid.addPoint(parts(0), parts(1), parts(2))
    result.addOne(grid)
    if buffered.hasNext then buffered.next // skip blank line
  result

val grids = parse(exampleInput)

def part1(): Int =
  // println(grids.mkString("\n"))
  val unvisitedLocked = MSet(1)
  val visited = MSet[Int]()
  // val unvisited = MSet.from[Int](1 until grids.length)
  val unvisited = MSet(0, 2, 3, 4)
  while visited.size != grids.size do
    println(
      s"top of loop, with visited=${visited} and unvisited=${unvisited} and unvisitedLocked=${unvisitedLocked}"
    )
    val referenceGridIdx = unvisitedLocked.head
    println(s"visiting ${referenceGridIdx}")
    // println(s"has grid ${grids(referenceGridIdx)}")
    unvisitedLocked.remove(referenceGridIdx)
    visited.add(referenceGridIdx)
    val referenceGrid = grids(referenceGridIdx)
    // TODO grid(1) is supposed match grid(4) but it doesn't
    for (grid, i) <- unvisited.map(i => (grids(i), i)) do
      println(s"trying ${i}")
      tryMatch(referenceGrid, grid) match
        case None => {}
        case Some(shift, o) => {
          println(s"found match ${i} with orientation ${o} and shift ${shift}")
          grids(i) = grid.inOrientation(o).shift(shift)
          // println(s"post-change grid ${grids(i)}")
          unvisitedLocked.add(i)
          unvisited.remove(i)
          assert(grids(referenceGridIdx).matches(grids(i)) == Some((0, 0, 0)))
        }

  val allPts = MSet[Point]()
  for
    grid <- grids
    pt <- grid.points
  do allPts.add(pt)
  allPts.size

def tryMatch(
    baseGrid: Grid,
    grid: Grid
): Option[((Int, Int, Int), Reorientation)] =
  LazyList
    .from[Reorientation](Reorientation.allReorienations())
    .map(o => (o, baseGrid.matches(grid.inOrientation(o))))
    .find(!_._2.isEmpty)
    .map((o, shift) => (shift.get, o))
