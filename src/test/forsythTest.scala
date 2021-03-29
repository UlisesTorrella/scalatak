import chess._
import scala.collection.mutable.Stack

object forsythTest {

  def test1 = {
    val situation = Situation(variant.Crazyhouse)
    val piece1 = Piece(Color.White, Flatstone)

    val board = Board(Map[Pos,Stack[Piece]](
      (Pos.A4 -> Stack(piece1)),
      (Pos.A5 -> Stack(piece1))),
      chess.variant.Crazyhouse)

    format.Forsyth >> Situation(board, Color.Black)
  }

  def test2 = {
    val situation = Situation(variant.Crazyhouse)
    val piece1 = Piece(Color.White, Flatstone)
    val piece2 = Piece(Color.Black, Flatstone)
    val board = Board(Map[Pos,Stack[Piece]](
      (Pos.A4 -> Stack(piece1, piece2)),
      (Pos.B5 -> Stack(piece2, piece1))),
      chess.variant.Crazyhouse)

    format.Forsyth >> Situation(board, Color.Black)
  }

  def test3 = {
    val situation = Situation(variant.Crazyhouse)
    val piece1 = Piece(Color.White, Flatstone)
    val piece2 = Piece(Color.Black, Flatstone)
    val piece3 = Piece(Color.Black, Capstone)
    val board = Board(Map[Pos,Stack[Piece]](
      (Pos.A4 -> Stack(piece1, piece2)),
      (Pos.B5 -> Stack(piece2, piece1)),
      (Pos.D5 -> Stack(piece3, piece2, piece1))),
      chess.variant.Crazyhouse)

    format.Forsyth >> Situation(board, Color.Black)
  }
}
