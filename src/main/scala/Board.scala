package chess

import variant.{ Variant, Standard }
import scala.collection.mutable.Stack
import Direction.Direction

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    crazyData: Option[Standard.Data] = None
) {

  def apply(at: Pos): Option[Stack[Piece]] = pieces get at

  def apply(file: File, rank: Rank): Option[Stack[Piece]] =
    pieces get Pos(file, rank)

  lazy val actors: Map[Pos, Actor] = pieces collect {
    case (pos, Stack(x, _*)) => (pos -> Actor(x, pos, this))
  }

  lazy val actorsOf: Color.Map[Seq[Actor]] = {
    val (w, b) = actors.values.toSeq.partition { _.color.white }
    Color.Map(w, b)
  }

  def rolesOf(c: Color): List[Role] =
    pieces.values
      .collect {
        case Stack(x, _*) if x.color == c => x role
      }
      .to(List)

  def actorAt(at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Color, stack: Stack[Piece]) =
    stack filter (_ is c) size

  def piecesOf(c: Color): Int = pieces.map { case (_, s) => piecesOf(c, s) } sum

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Option(this): Option[Board])(_ flatMap _)

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces contains at) pieces get at match {
      case Some(Stack()) => Option(copy(pieces = pieces - at + ((at, Stack(piece)))))
      case _             => None
    }
    else Option(copy(pieces = pieces + ((at, Stack(piece)))))

  def take(at: Pos): Option[Board] =
    if (pieces contains at) Option(copy(pieces = pieces - at))
    else None


  // Moving and taking have the same logic in tak, we always asume an empty stack
  def move(orig: Pos, dest: Pos, index: Int): Option[Board] =
    if (!(pieces contains orig)) None
    else
      for {
        stack  <- pieces get orig
        dstack = pieces.getOrElse(dest, Stack[Piece]())
        (movingPieces, leftPieces) = stack splitAt index
      } yield copy(pieces = pieces - dest - orig
                  + (dest -> (movingPieces ++ dstack) )
                  + (orig -> leftPieces))

  def move(orig: Pos, dir: Direction, index: Int, drops: List[Int]): Option[Board] =
    if (index==0) Option(this)
    else drops match {
      case d :: rest => for {
                          dest <- Direction(dir, orig)
                          b    <- move(orig, dest, index)
                          b2   <- b.move(dest, dir, index-d, rest)
                        } yield b2
      case Nil       => Option(this)
    }

  // def taking(orig: Pos, dest: Pos, index: Int = 0): Option[Board] =
  //   for {
  //     stack <- pieces get orig
  //     dstack = pieces.getOrElse(dest, Stack[Piece]())
  //     (movingPieces, leftPieces) = stack splitAt index
  //   } yield copy(pieces = pieces - dest - orig
  //               + (dest -> (movingPieces ++ dstack) )
  //               + (orig -> leftPieces))

  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { color =>
    pieces.collect { case (pos, Stack(x, _*)) if x is color => pos }.to(Set)
  }

  def hasPiece(p: Piece) = pieces.values exists { case Stack(x) => x == p }

  def promote(pos: Pos): Option[Board] = None

  def withHistory(h: History): Board = copy(history = h)

  def withPieces(newPieces: PieceMap) = copy(pieces = newPieces)

  def withVariant(v: Variant): Board = copy(variant = v).ensureCrazyData

  def withCrazyData(data: Standard.Data)         = copy(crazyData = Option(data))
  def withCrazyData(data: Option[Standard.Data]) = copy(crazyData = data)
  def withCrazyData(f: Standard.Data => Standard.Data): Board =
    withCrazyData(f(crazyData | Standard.Data.init))

  def ensureCrazyData = withCrazyData(crazyData | Standard.Data.init)

  def updateHistory(f: History => History) = copy(history = f(history))

  def count(p: Piece): Int = pieces.values map { case s => s count (_ == p) } sum
  def count(c: Color): Int = pieces.values map { case s => s count (_.color == c) } sum

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(this) || history.fivefoldRepetition

  def situationOf(color: Color) = Situation(this, color)

  def visual = format.Visual >> this

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  def emptySquares: List[Pos] =
    Pos.all diff pieces.filterNot(_._2 isEmpty).keys.toSeq

  def hasPathstoneAt(pos: Pos, c: Color) =
    if (pieces contains pos)
      pieces(pos) match {
        case Stack() => false
        case Stack(x, _*) => x.role match {
                           case _: Pathstone => x.color == c
                           case _ => false
                         }
      }
    else false

  def hasPath(color: Color): Boolean =
    (pieces.view.filterKeys { pos => (pos rank) == Rank.First } exists { case (pos, _) => hasPathUpFrom(pos, color, Set[Pos]()) }) ||
    (pieces.view.filterKeys { pos => (pos file) == File.A } exists { case (pos, _) => hasPathRightFrom(pos, color, Set[Pos]()) })

  def hasPathUpFrom(pos: Pos, color: Color, visited: Set[Pos]): Boolean =
    if (hasPathstoneAt(pos, color))
      if (pos.rank == Rank.Eighth) true
      else pos.takNeighboursUp filter { pos => !(visited contains pos) && hasPathstoneAt(pos, color) } exists { pos => hasPathUpFrom(pos, color, visited + pos) }
    else false

  def hasPathRightFrom(pos: Pos, color: Color, visited: Set[Pos]): Boolean =
    if (hasPathstoneAt(pos, color))
      if (pos.file == File.H) true
      else pos.takNeighboursRight filter { pos => !(visited contains pos) && hasPathstoneAt(pos, color) } exists { pos => hasPathRightFrom(pos, color, visited + pos) }
    else false

  override def toString = s"$variant Position after ${history.lastMove}\n$visual"
}

object Board {

  def apply(pieces: Iterable[(Pos, Stack[Piece])], variant: Variant): Board =
    new Board(pieces.toMap, History(), variant, variantCrazyData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, History(), variant, variantCrazyData(variant))

  def empty(variant: Variant): Board = Board(Map.empty[Pos, Stack[Piece]], variant)

  private def variantCrazyData(variant: Variant) = Option(Standard.Data.init)
}
