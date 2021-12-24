package test

import utest.*
import day22.*

object Day22Test extends TestSuite:
  val tests = Tests {
    test("intersection") {
      val cube1 = Cube.s(0, 2, 3, 5, 6, 8)
      assert(cube1.size == 3 * 3 * 3)
      assert(cube1.intersects(cube1))
      assert(!cube1.intersects(Cube.s(0, 2, 3, 5, 0, 5)))
      assert(cube1.intersects(Cube.s(0, 0, 3, 3, 0, 6)))
    }
    test("difference subset") {
      val cube1 = Cube.s(0, 2, 3, 5, 6, 8)
      assert(cube1.difference(cube1).size == 0)
      val cube2 = Cube.s(0, 0, 3, 4, 6, 7)
      val diff1 = cube1.difference(cube2)
      assert(diff1.map(_.size).sum == cube1.size - cube2.size)
      diff1
    }
    test("difference overlap") {
      val cube1 = Cube.s(0, 2, 3, 5, 6, 8)
      val cube2 = Cube.s(-2, 1, 3, 5, 8, 9)
      val overlap = Cube.s(0, 1, 3, 5, 8, 8)
      val diff1 = cube1.difference(cube2)
      assert(diff1.map(_.size).sum == cube1.size - overlap.size)
      diff1
    }
    test("disjoint") {
      val cubes = DisjointCubes()
      val cube1 = Cube.s(0, 2, 3, 5, 6, 8)
      cubes.addCube(cube1)
      assert(cubes.size == cube1.size)
      cubes.subtractCube(cube1)
      assert(cubes.size == 0)
      cubes.addCube(cube1)
      cubes.addCube(cube1)
      assert(cubes.size == cube1.size)
      cubes.addCube(Cube.s(0, 1, 3, 4, 7, 7)) // subset of existing cube
      assert(cubes.size == cube1.size, cubes.cubes.size == 1)
      cubes.addCube(Cube.s(0, 2, 3, 5, 7, 7)) // subset of existing cube
      assert(cubes.size == cube1.size, cubes.cubes.size == 1)
      cubes.subtractCube(Cube.s(0, 0, 3, 3, 6, 6))
      assert(cubes.size == cube1.size - 1)
      cubes.addCube(Cube.s(0, 0, 3, 3, 6, 6))
      assert(cubes.size == cube1.size)
      cubes.addCube(Cube.s(10, 10, 3, 3, 6, 6)) // disjoint from existing
      assert(cubes.size == cube1.size + 1)

      val overlapping = Cube.s(2, 4, 5, 7, 8, 10)
      cubes.addCube(overlapping) // overlaps by 1 with existing

      assert(cubes.size == cube1.size + overlapping.size)
      cubes.cubes
    }
    test("first small example") {
      /*
       on x=10..12,y=10..12,z=10..12
       on x=11..13,y=11..13,z=11..13
       off x=9..11,y=9..11,z=9..11
       on x=10..10,y=10..10,z=10..10
       */
      val cubes = DisjointCubes()
      cubes.addCube(Cube.s(10, 12, 10, 12, 10, 12))
      cubes.addCube(Cube.s(11, 13, 11, 13, 11, 13))
      cubes.subtractCube(Cube.s(9, 11, 9, 11, 9, 11))
      cubes.addCube(Cube.s(10, 10, 10, 10, 10, 10))
      assert(cubes.size == 39)
    }
    test("three cubes with no mutual overlap") {
      val cube1 = Cube.s(0, 2, 0, 2, 0, 2)
      val cube2 = Cube.s(2, 4, 2, 4, 2, 4)
      val cube3 = Cube.s(4, 6, 4, 6, 4, 6)
      val d = DisjointCubes()
      d.addCube(cube1)
      assert(d.size == 27)
      d.addCube(cube2)
      assert(d.size == 27 * 2 - 1)
      d.addCube(cube3)
      assert(d.size == 27 * 3 - 2)
    }
    test("third cubes overlaps two cubes") {
      val cube1 = Cube.s(0, 2, 0, 2, 0, 2)
      val cube2 = Cube.s(3, 4, 3, 4, 3, 4)
      val cube3 = Cube.s(2, 3, 2, 3, 2, 3)

      assert(cube1.intersection(cube2) == None)

      val overlap_1_3 = Cube.s(2, 2, 2, 2, 2, 2)
      assert(cube1.intersection(cube3) == Some(overlap_1_3))

      val overlap_2_3 = Cube.s(3, 3, 3, 3, 3, 3)
      assert(cube2.intersection(cube3) == Some(overlap_2_3))

      assert(
        cube2.intersection(cube3).get.intersection(cube1) == None
      )

      val d = DisjointCubes()
      d.addCube(cube1)
      assert(d.size == cube1.size)

      d.addCube(cube2)
      assert(d.size == cube1.size + cube2.size)

      d.addCube(cube3)
      assert(
        d.size == cube1.size + cube2.size + cube3.size -
          overlap_2_3.size - overlap_1_3.size
      )
    }
    test("three cubes with  mutual overlap") {
      val cube1 = Cube.s(0, 2, 0, 2, 0, 2)
      val cube2 = Cube.s(2, 4, 2, 4, 2, 4)
      val cube3 = Cube.s(0, 2, 2, 5, -5, 2)

      val overlap_1_2 = Cube.s(2, 2, 2, 2, 2, 2)
      assert(cube1.intersection(cube2) == Some(overlap_1_2))

      val overlap_1_3 = Cube.s(0, 2, 2, 2, 0, 2)
      assert(cube1.intersection(cube3) == Some(overlap_1_3))

      val overlap_2_3 = Cube.s(2, 2, 2, 4, 2, 2)
      assert(cube2.intersection(cube3) == Some(overlap_2_3))

      val overlap_1_2_3 = Cube.s(2, 2, 2, 2, 2, 2)
      assert(
        cube2.intersection(cube3).get.intersection(cube1) == Some(overlap_1_2_3)
      )

      val d = DisjointCubes()
      d.addCube(cube1)
      assert(d.size == cube1.size)

      d.addCube(cube2)
      assert(d.size == cube1.size + cube2.size - overlap_1_2.size)

      d.addCube(cube3)
      assert(
        d.size == cube1.size + cube2.size + cube3.size -
          overlap_1_2.size - overlap_2_3.size - overlap_1_3.size +
          overlap_1_2_3.size
      )
    }
//     test("part 1 example") {
//       val inp = """on x=-20..26,y=-36..17,z=-47..7
// on x=-20..33,y=-21..23,z=-26..28
// on x=-22..28,y=-29..23,z=-38..16
// on x=-46..7,y=-6..46,z=-50..-1
// on x=-49..1,y=-3..46,z=-24..28
// on x=2..47,y=-22..22,z=-23..27
// on x=-27..23,y=-28..26,z=-21..29
// on x=-39..5,y=-6..47,z=-3..44
// on x=-30..21,y=-8..43,z=-13..34
// on x=-22..26,y=-27..20,z=-29..19
// off x=-48..-32,y=26..41,z=-47..-37
// on x=-12..35,y=6..50,z=-50..-2
// off x=-48..-32,y=-32..-16,z=-15..-5
// on x=-18..26,y=-33..15,z=-7..46
// off x=-40..-22,y=-38..-28,z=23..41
// on x=-16..35,y=-41..10,z=-47..6
// off x=-32..-23,y=11..30,z=-14..3
// on x=-49..-5,y=-3..45,z=-29..18
// off x=18..30,y=-20..-8,z=-3..13
// on x=-41..9,y=-7..43,z=-33..15
// on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
// on x=967..23432,y=45373..81175,z=27513..53682
// """.linesIterator
//       val lines = inp.map(Line.parse).filter(_.isSmall).toList
//       val cubes = lines.map(Cube.fromLine)
//       assert(lines.size == 20)
//       val dcubes = DisjointCubes()
//       dcubes.addCube(cubes(0))
//       assert(dcubes.size == cubes(0).size)

//       dcubes.addCube(cubes(1))
//       val overlap_0_1 = Cube.s(-20, 26, -21, 17, -26, 7)
//       assert(cubes(0).intersection(cubes(1)).get == overlap_0_1)
//       assert(dcubes.size == cubes(0).size + cubes(1).size - overlap_0_1.size)

//       val overlap_0_2 = Cube.s(-20, 26, -29, 17, -38, 7)
//       assert(cubes(0).intersection(cubes(2)).get == overlap_0_2)
//       val overlap_1_2 = Cube.s(-20, 28, -21, 23, -26, 16)
//       assert(cubes(1).intersection(cubes(2)).get == overlap_1_2)
//       val overlap_0_1_2 = Cube.s(-20, 26, -21, 17, -26, 7)
//       assert(overlap_0_1.intersection(overlap_1_2).get == overlap_0_1_2)
//       dcubes.addCube(cubes(2))
//       assert(
//         dcubes.size ==
//           cubes(0).size + cubes(1).size + cubes(2).size -
//           overlap_0_1.size - overlap_1_2.size - overlap_0_2.size +
//           overlap_0_1_2.size
//       )

//       dcubes.cubes
//       // for line <- lines do
//       //   val cube = Cube.fromLine(line)
//       //   if line.on then cubes.addCube(cube)
//       //   else cubes.subtractCube(cube)
//       // assert(cubes.size == 590784)
//       // dcubes.size
//     }
  }
