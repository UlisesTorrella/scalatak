package chess


import org.specs2.mutable.Specification
import scala.collection.mutable.Stack

class ForsythTest extends Specification {

  "A4 A5" should {
    val situation = Situation(variant.Crazyhouse)
    val piece1 = Piece(Color.White, Flatstone)

    val board = Board(Map[Pos,Stack[Piece]](
      (Pos.A4 -> Stack(piece1)),
      (Pos.A5 -> Stack(piece1))),
      chess.variant.Crazyhouse)

    val fen = "8/8/8/(F)7/(F)7/8/8/8/FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWffffffffffffffffffffffffffffffffffffffffffffffffffccwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww b - 0 1"
    "format" in {
      format.FEN(fen)==format.Forsyth >> Situation(board, Color.Black)
    }
  }

  "Stacked on a4 and b5" should {
    val situation = Situation(variant.Crazyhouse)
    val piece1 = Piece(Color.White, Flatstone)
    val piece2 = Piece(Color.Black, Flatstone)
    val board = Board(Map[Pos,Stack[Piece]](
      (Pos.A4 -> Stack(piece1, piece2)),
      (Pos.B5 -> Stack(piece2, piece1))),
      chess.variant.Crazyhouse)

    val fen = "8/8/8/1(fF)6/(Ff)7/8/8/8/FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWffffffffffffffffffffffffffffffffffffffffffffffffffccwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww b - 0 1"
    "format" in {
      format.FEN(fen)==format.Forsyth >> Situation(board, Color.Black)
    }
  }

  "Stacked on a4, b5 and d5 " should {
    val situation = Situation(variant.Crazyhouse)
    val piece1 = Piece(Color.White, Flatstone)
    val piece2 = Piece(Color.Black, Flatstone)
    val piece3 = Piece(Color.Black, Capstone)
    val board = Board(Map[Pos,Stack[Piece]](
      (Pos.A4 -> Stack(piece1, piece2)),
      (Pos.B5 -> Stack(piece2, piece1)),
      (Pos.D5 -> Stack(piece3, piece2, piece1))),
      chess.variant.Crazyhouse)

    val fen = "8/8/8/1(fF)1(cfF)4/(Ff)7/8/8/8/FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWffffffffffffffffffffffffffffffffffffffffffffffffffccwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww b - 0 1"

    "format" in {
      format.FEN(fen)==format.Forsyth >> Situation(board, Color.Black)
    }
  }
}
