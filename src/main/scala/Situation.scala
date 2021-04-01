package chess

import cats.data.Validated
import cats.implicits._

import chess.format.Uci

import Direction._

case class Situation(board: Board, color: Color) {

  lazy val actors = board actorsOf color

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val playerCanCapture: Boolean = moves exists (_._2 exists (_.captures))

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map { move: Move => Direction(move.dir, move.orig)} flatten }.to(Map)

  def drops: Option[List[Pos]] = board.variant canDropStuff this

  lazy val check: Boolean = false

  def history = board.history

  def checkMate: Boolean = board.variant checkmate this

  def staleMate: Boolean = board.variant staleMate this

  def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  def opponentHasInsufficientMaterial: Boolean = board.variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  def variantEnd = board.variant specialEnd this

  def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(color = !color).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  def move(index:Int, from: Pos, dir: Direction, drops: List[Int]): Validated[String, Move] =
    board.variant.move(this, index, from, dir, drops)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.i, uci.orig, uci.dir, uci.drops)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: chess.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def enPassantSquare: Option[Pos] = None

  def unary_! = copy(color = !color)
}

object Situation {

  def apply(variant: chess.variant.Variant): Situation = Situation(Board init variant, White)
}
