package chess

import chess.format.Uci
import cats.syntax.option._

import Direction._

case class Move(
    piece: Piece,
    orig: Pos,
    dir: Direction,
    situationBefore: Situation,
    after: Board,
    capture: Option[Pos],
    promotion: Option[PromotableRole],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics(),
    stackIndex: Int = 0, // by default at top
    drops: List[Int] = Nil // empty drop list will be considered as "all stones dropped in the first square"
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

  def firstStep: Pos = Direction(dir, orig) match {
    case Some(x) => x
    case None    => orig
  }

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def color = piece.color

  def withAfter(newBoard: Board) = copy(after = newBoard)

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def withIndex(i: Int) = copy(stackIndex=i)

  def withDrops(drops: List[Int]) = copy(drops=drops)

  def toUci = Uci.Move(orig, dir, stackIndex, drops)

  override def toString = s"$piece ${toUci.uci}"
}
