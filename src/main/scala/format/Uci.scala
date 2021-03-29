package chess
package format

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, Pos)

  def apply(situation: Situation): Validated[String, MoveOrDrop]
}

object Uci {

  case class Move(
      orig: Pos,
      dest: Pos,
      i: Int = 1
  ) extends Uci {

    def keys = orig.key + dest.key + i.toString
    def uci  = keys

    def keysPiotr = orig.piotrStr + dest.piotrStr + i.toString
    def piotr     = keysPiotr

    def origDest = orig -> dest

    def apply(situation: Situation) = situation.move(orig, dest, i) map Left.apply
  }

  object Move {

    def apply(move: String): Option[Move] =
      if ( move.size < 4) None
      else
        for {
          orig <- Pos.fromKey(move take 2)
          dest <- Pos.fromKey(move.slice(2, 4))
          i    = "1"//move lift 4 getOrElse "0"
        } yield Move(orig, dest, i.toInt)

    def piotr(move: String) =
      if ( move.size < 2) None
      else
        for {
          orig <- move.headOption flatMap Pos.piotr
          dest <- move lift 1 flatMap Pos.piotr
          i    = "1"//move(2)
        } yield Move(orig, dest, i.toInt)

    def fromStrings(origS: String, destS: String, i: Int = 0) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
      } yield Move(orig, dest, i)
  }

  case class Drop(role: Role, pos: Pos) extends Uci {

    def uci = s"${role.pgn}@${pos.key}"

    def piotr = s"${role.pgn}@${pos.piotrStr}"

    def origDest = pos -> pos

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

  def apply(move: chess.Move) = Uci.Move(move.orig, move.dest, move.stackIndex)

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
