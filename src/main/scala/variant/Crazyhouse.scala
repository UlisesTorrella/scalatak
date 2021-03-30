package chess
package variant

import chess.format.Uci
import cats.syntax.option._
import cats.data.Validated
import chess.format.FEN

case object Crazyhouse
    extends Variant(
      id = 10,
      key = "crazyhouse",
      name = "Crazyhouse",
      shortName = "Crazy",
      title = "The goal of Tak is to be the first to connect two opposite edges of the board with pieces called stones, and create a road.",
      standardInitialPosition = true
    ) {

  def pieces = Standard.pieces

  override val initialFen = FEN("8/8/8/8/8/8/8/8/FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWffffffffffffffffffffffffffffffffffffffffffffffffffccwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww w - - 0 1")

  override def valid(board: Board, strict: Boolean) = true

  override def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    for {
      d1 <- situation.board.crazyData toValid "Board has no crazyhouse data"
      piece = Piece(situation.color, role)
      d2     <- d1.drop(piece) toValid s"No $piece to drop on $pos"
      board1 <- situation.board.place(piece, pos) toValid s"Can't drop $role on $pos, it's occupied"
    } yield Drop(
      piece = piece,
      pos = pos,
      situationBefore = situation,
      after = board1 withCrazyData d2
    )

  override def fiftyMoves(history: History): Boolean = false

  override def isIrreversible(move: Move): Boolean = false

  override def finalizeBoard(board: Board, uci: Uci): Board = board

  // Checkmate here means a path has been stablished
  override def checkmate(situation: Situation) = situation.board hasPath !situation.color


  override def canDropStuff(situation: Situation): Option[List[Pos]] =
    situation.board.crazyData match {
      case Some(data) => if (data.pockets(situation.color).roles.nonEmpty) Some(situation.board.emptySquares) else None
      case None => None
    }
  override def staleMate(situation: Situation) = false
  // there is always sufficient mating material in Crazyhouse
  override def opponentHasInsufficientMaterial(situation: Situation) = false
  override def isInsufficientMaterial(board: Board)                  = false

  val storableRoles = List(Flatstone, Capstone, Wallstone)

  case class Data(
      pockets: Pockets,
  ) {

    def drop(piece: Piece): Option[Data] =
      pockets take piece map { nps =>
        copy(pockets = nps)
      }

    def store(piece: Piece, from: Pos) =
      copy(
        pockets = pockets store piece
      )
  }

  object Data {
    val init = Data(
      Pockets(
        Pocket(List.fill(50)(Flatstone) ++ List.fill(2)(Capstone) ++ List.fill(50)(Wallstone)),
        Pocket(List.fill(50)(Flatstone) ++ List.fill(2)(Capstone) ++ List.fill(50)(Wallstone))
      )
    )
  }

  case class Pockets(white: Pocket, black: Pocket) {

    def apply(color: Color) = color.fold(white, black)

    def take(piece: Piece): Option[Pockets] =
      piece.color.fold(
        white take piece.role map { np =>
          copy(white = np)
        },
        black take piece.role map { np =>
          copy(black = np)
        }
      )

    def store(piece: Piece) =
      piece.color.fold(
        copy(black = black store piece.role),
        copy(white = white store piece.role)
      )
  }

  case class Pocket(roles: List[Role]) {

    def take(role: Role) =
      // Taking a flatstone or a Wallstone consumes both
      role match {
        case Wallstone => if ((roles contains role) && (roles contains Flatstone)) Option(copy(roles = roles diff List(role) diff List(Flatstone)))
                          else None
        case Flatstone => if ((roles contains role) && (roles contains Wallstone)) Option(copy(roles = roles diff List(role) diff List(Wallstone)))
                          else None
        case _ => if (roles contains role) Option(copy(roles = roles diff List(role)))
                         else None
      }



    def store(role: Role) =
      if (storableRoles contains role) copy(roles = role :: roles)
      else this
  }
}
