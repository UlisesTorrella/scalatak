package chess
package format.pgn
import scala.collection.mutable.Stack
object Dumper {

  def apply(situation: Situation, data: chess.Move, next: Situation): String = {
    import data._

    ((piece.role) match {
      case (_) => s"""${stackIndex}${orig}${dir.toString}${dropsS}""" // PGN and UCI are the same here, sorry
    }) + {
      if (next.check) {
        if (next.checkMate) "#" else "+"
      } else if (next.winner.isDefined) "#"
      else ""
    }
  }

  def apply(data: chess.Drop, next: Situation): String = {
    data.toUci.uci + {
      if (next.check) {
        if (next.checkMate) "#" else "+"
      } else if (next.winner.isDefined) "#"
      else ""
    }
  }

  def apply(data: chess.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.color
    )

  def apply(data: chess.Drop): String =
    apply(
      data,
      data.finalizeAfter situationOf !data.color
    )
}
