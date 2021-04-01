package chess
package format

import Direction.Direction

case class UciCharPair(a: Char, b: Char) {

  override def toString = s"$a$b"
}

object UciCharPair {

  import implementation._

  def apply(uci: Uci): UciCharPair =
    uci match {
      case Uci.Move(orig, dir, i, drops)       => UciCharPair(toChar(orig), toChar(dir))
      case Uci.Drop(role, pos) =>
        UciCharPair(
          toChar(pos),
          dropRole2charMap.getOrElse(role, voidChar)
        )
    }

  private[format] object implementation {

    val charShift = 35        // Start at Char(35) == '#'
    val voidChar  = 33.toChar // '!'. We skipped Char(34) == '"'.

    val pos2charMap: Map[Pos, Char] = Pos.all
      .map { pos =>
        pos -> (pos.hashCode + charShift).toChar
      }
      .to(Map)

    def toChar(pos: Pos) = pos2charMap.getOrElse(pos, voidChar)

    def toChar(dir: Direction) = Direction.toChar(dir)

    val dropRole2charMap: Map[Role, Char] =
      Role.all
        .filterNot(King ==)
        .zipWithIndex
        .map { case (role, index) =>
          role -> (charShift + pos2charMap.size + index).toChar
        }
        .to(Map)
  }
}
