package chess

import chess.format.Uci
import cats.syntax.option._

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    capture: Option[Pos],
    promotion: Option[PromotableRole],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics(),
    stackIndex: Int = 0 // by default at top
) {
  def before = situationBefore.board

  def situationAfter = Situation(finalizeAfter, !piece.color)

  def withHistory(h: History) = copy(after = after withHistory h)

  def finalizeAfter: Board = {
    val board = after updateHistory { h1 =>
      h1.copy(
        lastMove = Option(toUci),
        halfMoveClock =
          if ((piece is Pawn) || captures || promotes) 0
          else h1.halfMoveClock + 1
      )
    }

    board.variant.finalizeBoard(board, toUci) updateHistory { h =>
      lazy val positionHashesOfSituationBefore =
        if (h.positionHashes.isEmpty) Hash(situationBefore) else h.positionHashes
      val resetsPositionHashes = board.variant.isIrreversible(this)
      val basePositionHashes =
        if (resetsPositionHashes) Array.empty: PositionHash else positionHashesOfSituationBefore
      h.copy(positionHashes = Hash(Situation(board, !piece.color)) ++ basePositionHashes)
    }
  }

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def color = piece.color

  def withPromotion(op: Option[PromotableRole]): Option[Move] =
    op.fold(this.some) { p =>
      if ((after count color.queen) > (before count color.queen)) for {
        b2 <- after take dest
        b3 <- b2.place(color - p, dest)
      } yield copy(after = b3, promotion = Option(p))
      else this.some
    }

  def withAfter(newBoard: Board) = copy(after = newBoard)

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def withIndex(i: Int) = copy(stackIndex=i)

  def toUci = Uci.Move(orig, dest, stackIndex)

  override def toString = s"$piece ${toUci.uci}"
}
