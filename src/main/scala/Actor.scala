package chess

import scala.annotation.tailrec
import scala.collection.mutable.{ ArrayBuffer, Stack }

import Direction._

final case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board
) {

  import Actor._

  lazy val moves: List[Move] = trustedMoves()

  /** The moves without taking defending the king into account */
  def trustedMoves(): List[Move] = {
    val moves = piece.role match {
      case Pawn => Nil // too much code there are no more pawns

      case Bishop => shortRange(Bishop.dirs)

      case Knight => shortRange(Knight.dirs)

      case Rook => shortRange(Rook.dirs)

      case Queen => shortRange(Queen.dirs)

      case King               => shortRange(King.dirs)
      case Capstone           => shortRange(Flatstone.dirs)
      case Flatstone          => shortRange(Flatstone.dirs)
      case Wallstone          => shortRange(Flatstone.dirs)
    }

    // We apply the current game variant's effects if there are any so that we can accurately decide if the king would
    // be in danger after the move was made.
    if (board.variant.hasMoveEffects) moves map (_.applyVariantEffect) else moves
  }

  lazy val destinations: List[Pos] = moves map { move: Move => Direction(move.dir, pos) } flatten

  def color        = piece.color
  def is(c: Color) = c == piece.color
  def is(r: Role)  = r == piece.role
  def is(p: Piece) = p == piece

  private def shortRange(dirs: List[Direction]): List[Move] =
    dirs flatMap { dir => //_(pos) } flatMap { to =>
      for {
        to <- Direction(dir, pos)
      }
      yield
        board.pieces.get(to) match {
          case None           => board.move(pos, to) map { move(dir, _) }
          case Some(stack)    => stack match {
            case Stack(p, _*) => if (p.role == Flatstone) board.move(pos, to) map { move(dir, _) }
                                 else Nil
            case Stack() => board.move(pos, to) map { move(dir, _) }
            case _ => Nil
          }
        }
    } flatten


  private def move(
      dir: Direction,
      after: Board,
      capture: Option[Pos] = None,
      promotion: Option[PromotableRole] = None,
      enpassant: Boolean = false
  ) =
    Move(
      piece = piece,
      orig = pos,
      dir = dir,
      situationBefore = Situation(board, piece.color),
      after = after,
      capture = capture,
      promotion = promotion,
      enpassant = enpassant
    )

}
