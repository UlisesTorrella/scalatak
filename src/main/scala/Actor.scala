package chess

import scala.annotation.tailrec
import scala.collection.mutable.{ ArrayBuffer, Stack }

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

      case Bishop => longRange(Bishop.dirs)

      case Knight => shortRange(Knight.dirs)

      case Rook => longRange(Rook.dirs)

      case Queen => longRange(Queen.dirs)

      case King               => shortRange(King.dirs)
      case Capstone           => shortRange(Flatstone.dirs)
      case Flatstone          => shortRange(Flatstone.dirs)
      case Wallstone          => shortRange(Flatstone.dirs)
    }

    // We apply the current game variant's effects if there are any so that we can accurately decide if the king would
    // be in danger after the move was made.
    if (board.variant.hasMoveEffects) moves map (_.applyVariantEffect) else moves
  }

  lazy val destinations: List[Pos] = moves map (_.dest)

  def color        = piece.color
  def is(c: Color) = c == piece.color
  def is(r: Role)  = r == piece.role
  def is(p: Piece) = p == piece

  private def shortRange(dirs: Directions): List[Move] =
    dirs flatMap { _(pos) } flatMap { to =>
      board.pieces.get(to) match {
        case None => board.move(pos, to) map { move(to, _) }
        case Some(stack) => stack match {
          case Stack(p, _*) => if (p.role == Flatstone) board.move(pos, to) map { move(to, _) }
                               else Nil
          case Stack() => board.move(pos, to) map { move(to, _) }
          case _ => Nil
        }
      }
    }

  private def longRange(dirs: Directions): List[Move] = {
    val buf = new ArrayBuffer[Move]

    @tailrec
    def addAll(p: Pos, dir: Direction): Unit = {
      dir(p) match {
        case None => ()
        case s @ Some(to) =>
          board.pieces.get(to) match {
            case None =>
              board.move(pos, to).foreach { buf += move(to, _) }
              addAll(to, dir)
            case Some(stack) => stack match {
              case Stack(p, _*) => if (p.role == Flatstone) board.move(pos, to) map { move(to, _) }
                                   else Nil
              case _ => Nil
            }
          }
      }
    }

    dirs foreach { addAll(pos, _) }
    buf.toList
  }

  private def move(
      dest: Pos,
      after: Board,
      capture: Option[Pos] = None,
      promotion: Option[PromotableRole] = None,
      enpassant: Boolean = false
  ) =
    Move(
      piece = piece,
      orig = pos,
      dest = dest,
      situationBefore = Situation(board, piece.color),
      after = after,
      capture = capture,
      promotion = promotion,
      enpassant = enpassant
    )

}

object Actor {

  def longRangeThreatens(board: Board, p: Pos, dir: Direction, to: Pos): Boolean =
    board.variant.longRangeThreatens(board, p, dir, to)

  def pawnDirOf(color: Color): Direction = color.fold(_.up, _.down)

  /** Determines the position one ahead of a pawn based on the color of the piece.
    * White pawns move up and black pawns move down.
    */
  def posAheadOfPawn(pos: Pos, color: Color): Option[Pos] = pawnDirOf(color)(pos)

  /** Determines the squares that a pawn attacks based on the colour of the pawn.
    */
  def pawnAttacks(pos: Pos, color: Color): List[Pos] =
    color
      .fold(
        List(pos.upLeft, pos.upRight),
        List(pos.downLeft, pos.downRight)
      )
      .flatten
}
