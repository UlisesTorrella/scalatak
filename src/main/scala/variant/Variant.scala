package chess
package variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn
import scala.collection.mutable.Stack
import chess.format.FEN

import Direction.Direction
// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String,
    val standardInitialPosition: Boolean
) {

  def pieces: Map[Pos, Stack[Piece]]

  def standard      = this == Standard
  def fromPosition  = this == FromPosition

  def crazyhouse    = this == Crazyhouse

  def exotic = true // tak is always exotic!

  protected val backRank = Vector(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

  def initialFen: FEN = format.Forsyth.initial

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    situation.actors
      .collect {
        case actor if actor.moves.nonEmpty => actor.pos -> actor.moves
      }
      .to(Map)

  def kingSafety(m: Move, filter: Piece => Boolean, kingPos: Option[Pos]): Boolean = true

  def move(
      situation: Situation,
      index: Int,
      from: Pos,
      dir: Direction,
      drops: List[Int]
  ): Validated[String, Move] = {

    // Find the move in the variant specific list of valid moves
    def findMove(from: Pos, dir: Direction) =
      situation.moves get from flatMap (_.find(_.dir == dir))

    for {
      actor <- situation.board.actors get from toValid "No piece on " + from
      _ <-
        if (actor is situation.color) Validated.valid(actor)
        else Validated.invalid("Not my piece on " + from)
      m1 <- findMove(from, dir) toValid "Piece on " + from + " cannot move to " + dir
    } yield (m1 withIndex index) withDrops drops
  }

  def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    Validated.invalid(s"$this variant cannot drop $situation $role $pos")

  def staleMate(situation: Situation): Boolean = !situation.check && situation.moves.isEmpty

  def checkmate(situation: Situation) = situation.check && situation.moves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Color] =
    if (situation.checkMate)
      if (situation.board.hasPath(Color.Black)) Some(Color.Black)
      else Some(Color.White)
    else None

  @nowarn def specialEnd(situation: Situation) = false

  @nowarn def specialDraw(situation: Situation) = false


  def canDropStuff(situation: Situation): Option[List[Pos]] = None
  /** Returns the material imbalance in pawns (overridden in Antichess)
    */
  def materialImbalance(board: Board): Int = board.count(Color.White) - board.count(Color.Black)

  /** Returns true if neither player can win. The game should end immediately.
    */
  def isInsufficientMaterial(board: Board) = false

  /** Returns true if the other player cannot win. This is relevant when the
    * side to move times out or disconnects. Instead of losing on time,
    * the game should be drawn.
    */
  def opponentHasInsufficientMaterial(situation: Situation) = false

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move, for
    * example
    */
  def addVariantEffect(move: Move): Move = move

  def fiftyMoves(history: History): Boolean = history.halfMoveClock >= 100

  def isIrreversible(move: Move): Boolean = false

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(board: Board, uci: format.Uci): Board = board

  def valid(board: Board, strict: Boolean) = true

  val roles = List(Rook, Knight, King, Bishop, King, Queen, Pawn, Flatstone, Wallstone, Capstone)

  val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight)

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  lazy val rolesPromotableByPgn: Map[Char, PromotableRole] =
    promotableRoles
      .map { r =>
        (r.pgn, r)
      }
      .to(Map)

  def isUnmovedPawn(color: Color, pos: Pos) = pos.rank == color.fold(Rank.Second, Rank.Seventh)

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id
}

object Variant {

  val all = List(
    Standard,
    Crazyhouse,
    FromPosition
  )
  val byId = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Standard

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(
    chess.variant.Standard,
    chess.variant.Crazyhouse
  )

  val divisionSensibleVariants: Set[Variant] = Set(
    chess.variant.Standard,
    chess.variant.FromPosition
  )

  private[variant] def symmetricRank(rank: IndexedSeq[Role]): Map[Pos, Piece] =
    (for (y <- Seq(Rank.First, Rank.Second, Rank.Seventh, Rank.Eighth); x <- File.all) yield {
      Pos(x, y) -> (y match {
        case Rank.First   => White - rank(x.index)
        case Rank.Second  => White.pawn
        case Rank.Seventh => Black.pawn
        case Rank.Eighth  => Black - rank(x.index)
      })
    }).toMap
}
