package chess

import Direction.Direction

sealed trait Role {
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char          = forsythUpper
  lazy val name               = toString.toLowerCase
  val projection: Boolean
  val dirs: List[Direction]
  def dir(from: Pos, to: Pos): Option[Direction]
}
sealed trait PromotableRole extends Role

trait Pathstone extends Role
object Pathstone {}
/** Promotable in antichess.
  */
case object King extends PromotableRole {
  val forsyth                 = 'k'
  val dirs: List[Direction]        = Flatstone.dirs
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

// Tak piece
case object Flatstone extends Pathstone {
  val forsyth                 = 'f'
  val dirs: List[Direction] = List(
    Direction.Up,
    Direction.Down,
    Direction.Left,
    Direction.Right
  )
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

// Tak piece
case object Capstone extends Pathstone {
  val forsyth                 = 'c'
  val dirs: List[Direction]   = Flatstone.dirs
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

// Tak piece
case object Wallstone extends Role {
  val forsyth                 = 'w'
  val dirs: List[Direction]        = Flatstone.dirs
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

case object Queen extends PromotableRole {
  val forsyth                 = 'q'
  val dirs: List[Direction]        = Flatstone.dirs
  def dir(from: Pos, to: Pos) = Rook.dir(from, to) orElse Bishop.dir(from, to)
  val projection              = true
}
case object Rook extends PromotableRole {
  val forsyth          = 'r'
  val dirs: List[Direction] = Flatstone.dirs
  def dir(from: Pos, to: Pos) = None
  val projection = true
}
case object Bishop extends PromotableRole {
  val forsyth          = 'b'
  val dirs: List[Direction] = Flatstone.dirs
  def dir(from: Pos, to: Pos) = None
  val projection = true
}
case object Knight extends PromotableRole {
  val forsyth = 'n'
  val dirs: List[Direction] = Flatstone.dirs
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}
case object Pawn extends Role {
  val forsyth                 = 'p'
  val dirs: List[Direction]        = Nil
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

object Role {

  val all: List[Role]                     = List(King, Queen, Rook, Bishop, Knight, Pawn, Flatstone, Capstone, Wallstone)
  val allPromotable: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)
  val allByForsyth: Map[Char, Role] = all map { r =>
    (r.forsyth, r)
  } toMap
  val allByPgn: Map[Char, Role] = all map { r =>
    (r.pgn, r)
  } toMap
  val allByName: Map[String, Role] = all map { r =>
    (r.name, r)
  } toMap
  val allPromotableByName: Map[String, PromotableRole] =
    allPromotable map { r =>
      (r.toString, r)
    } toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] =
    allPromotable map { r =>
      (r.forsyth, r)
    } toMap
  val allPromotableByPgn: Map[Char, PromotableRole] =
    allPromotable map { r =>
      (r.pgn, r)
    } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

  def valueOf(r: Role): Option[Int] =
    r match {
      case Pawn   => Option(1)
      case Knight => Option(3)
      case Bishop => Option(3)
      case Rook   => Option(5)
      case Queen  => Option(9)
      case King   => None
      case Flatstone => Option(1)
      case Wallstone => Option(2)
      case Capstone  => Option(3)
    }
}
