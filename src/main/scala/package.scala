import ornicar.scalalib
import scala.collection.mutable.Stack

package object chess extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val White = Color.White
  val Black = Color.Black

  type PieceMap = Map[Pos, Stack[Piece]]

  type PositionHash = Array[Byte]

  type MoveOrDrop = Either[Move, Drop]
}
