package streams

import org.junit._
import org.junit.Assert.assertEquals

import Bloxorz._

class BloxorzSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        if (!block.isLegal) {
          println("Illegal block " + block + " " + move)
        } else {
          println(block, move)
        }
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait LevelNoSol extends SolutionChecker {
    /* terrain for level 1 without solution*/

    val level =
      """ooo-------
        |oS-ooo----
        |o-ooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }


  @Test def `pathsFromStart produces only legal blocks`: Unit =
    new Level1 {
      pathsFromStart.forall(_._1.isLegal)
      pathsToGoal.forall(_._1.isLegal)
    }

  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }

  @Test def `find end pos char`: Unit = {
    new Level1 {
      val customLevel = Vector(level.split("\r?\n").map(str => Vector(str: _*)).toIndexedSeq: _*)
      assertEquals(findChar('T', customLevel), goal)
    }
  }

  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }

  @Test def `neighbors with history works with initial state`: Unit =
    new Level1 {
      val nwh = neighborsWithHistory(startBlock, List())
      val actual = newNeighborsOnly(nwh, Set()).toSet
      val expected = Set((Block(Pos(2,1),Pos(3,1)),List(Down)), (Block(Pos(1,2),Pos(1,3)),List(Right)))
      assertEquals(expected, actual)
    }

  @Test def `move test on my terrain`: Unit =
    new Level1 {
      assert(!Block(Pos(1, 1), Pos(1, 1)).up.isLegal)
    }

  @Test def `fromStart contains goal position`: Unit =
    new Level1 {
      assert(pathsFromStart.map(_._1).contains(Block(goal, goal)))
    }

  @Test def `toGoal contains goal position`: Unit =
    new Level1 {
      assert(pathsToGoal.map(_._1).contains(Block(goal, goal)))
    }

  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }


  @Test def `neighbors check`: Unit = {
    new Level1 {
      val result = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val expected = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )

      assertEquals(result.toSet, expected)
    }
  }

  @Test def `new neighbors only`: Unit = {
    new Level1 {
      val actual = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).to(LazyList),

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      ).toSet

      val expected = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )

      assertEquals(expected, actual)
    }
  }

  @Test def `is standing false for non standing block`: Unit = {
    new Level1 {
      assert(!Block(Pos(2,1),Pos(3,1)).isStanding)
      assert(Block(Pos(2,1),Pos(2,1)).isStanding)
    }
  }

  @Test def `test up is correct`: Unit = {
    new Level1 {
      val block = Block(Pos(2,1),Pos(3,1))
      assertEquals(block.up, block.deltaRow(-1, -2))
    }
  }

  @Test def `no solution should return Nil`: Unit = {
    new LevelNoSol {
      assert(solution == Nil)
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
