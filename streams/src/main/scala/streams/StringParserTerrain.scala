package streams

import streams.Bloxorz.Level

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
 * inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
 * inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef:

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   *
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   *
   * is represented as
   *
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
  /*def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    def isinTerrain(pos: Pos):Boolean={
      if (levelVector.length >= pos.row)
        val terrain_row = levelVector(pos.row)
        if (terrain_row.length >= pos.col)
          val terrain_col = terrain_row(pos.col)
          if (List('S','T','o').contains(terrain_col)) true else false
        else false
      else false
    }
    isinTerrain
  }*/
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    def isValidPosition(input: Pos): Boolean = {
      try{
        levelVector(input.row)(input.col) match {
          case 'S' => true
          case 'T' => true
          case 'o' => true
          case  _ => false
        }
      } catch {
        case e: IndexOutOfBoundsException => false
      }
    }
    return isValidPosition
  }

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val pos = (for (row <- levelVector.indices;
          col <- levelVector(row).indices if levelVector(row)(col) == c) yield (row,col))
    val returned_value:Pos=Pos(pos.head._1,pos.head._2)
    returned_value
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\r?\n").map(str => Vector(str*)).toIndexedSeq*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

