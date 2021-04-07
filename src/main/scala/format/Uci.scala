package chess
package format

import cats.data.Validated
import cats.implicits._

import Direction.Direction

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, String)

  def apply(situation: Situation): Validated[String, MoveOrDrop]
}

// https://ustak.org/portable-tak-notation/
object Uci {

  case class Move(
      orig: Pos,
      dir: Direction,
      i: Int,
      drops: List[Int]
  ) extends Uci {

    def keys = i.toString + orig.key + dir.toString + drops.map(_.toString).mkString("")
    def uci  = keys

    def keysPiotr = i.toString + orig.piotrStr + dir.toString + drops.map(_.toString).mkString("")
    def piotr     = keysPiotr

    def origDest = orig -> dir.toString

    def apply(situation: Situation) = situation.move(i, orig, dir, drops) map Left.apply
  }

  object Move {

    def drops(dropsS: String): List[Int] = dropsS.map(_.toInt - 48).toList

    def apply(move: String): Option[Move] =
      if ( move.size < 4) None
      else
        for {
          i      <- move lift 0
          orig   <- Pos.fromKey(move.substring(1,3))
          dir    <- move lift 3
          dropsS = move drop 4
        } yield Move(orig, Direction(dir), i.toInt - 48, drops(dropsS))

    def piotr(move: String) =
      if ( move.size < 3) None
      else
        for {
          i     <- move lift 0
          orig  <- move lift 1 flatMap Pos.piotr
          dir   <- move lift 2
          dropsS = move drop 3
        } yield Move(orig, Direction(dir), i.toInt - 48, drops(dropsS))

    def fromStrings(origS: String, dir: String, i: Int, dropsS: String) =
      for {
        orig <- Pos.fromKey(origS)
      } yield Move(orig, Direction(dir), i, drops(dropsS))
  }

  case class Drop(role: Role, pos: Pos) extends Uci {

    def uci = s"${role.pgn}@${pos.key}"

    def piotr = s"${role.pgn}@${pos.piotrStr}"

    def origDest = pos -> "@"

    def apply(situation: Situation) = situation.drop(role, pos) map Right.apply
  }

  object Drop {

    def fromStrings(roleS: String, posS: String) =
      for {
        role <- Role.allByName get roleS
        pos  <- Pos.fromKey(posS)
      } yield Drop(role, pos)
  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: chess.Move) = Uci.Move(move.orig, move.dir, move.stackIndex, move.drops)

  def apply(drop: chess.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(move: String): Option[Uci] =
    if (move lift 1 contains '@') for {
      role <- move.headOption flatMap Role.allByPgn.get
      pos  <- Pos.fromKey(move.slice(2, 4))
    } yield Uci.Drop(role, pos)
    else Uci.Move(move)

  def piotr(move: String): Option[Uci] =
    if (move lift 1 contains '@') for {
      role <- move.headOption flatMap Role.allByPgn.get
      pos  <- move lift 2 flatMap Pos.piotr
    } yield Uci.Drop(role, pos)
    else Uci.Move.piotr(move)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
