package chess


import org.specs2.mutable.Specification
import chess.Board
import chess.Pos
import chess.Piece
import chess.Color
import chess.Flatstone
import scala.collection.mutable.Stack

class MoveTest extends Specification {
  val piece1 = Piece(Color.White, Flatstone)
  val board = Board(Map[Pos,Stack[Piece]]((Pos.A4 -> Stack(piece1)), (Pos.A5 -> Stack(piece1))), chess.variant.Crazyhouse)

  "moving from a4 to a5" should {
    "leave the original square" in {
      board.move(Pos.A4, Pos.A5, 1) match {
        case Some(board) => board.pieces.get(Pos.A4) match {
          case Some(Stack(_, _*)) => false
          case _ => true
        }
        case None => false
      }
    }
    "stack itself" in {
      board.move(Pos.A4, Pos.A5, 1) match {
        case Some(board) => board.pieces.get(Pos.A5) match {
          case Some(Stack(w, b)) => {
            println(s"stack: $w, $b")
            true
          }
          case _ => false
        }
        case None => false
      }
    }
  }

}
