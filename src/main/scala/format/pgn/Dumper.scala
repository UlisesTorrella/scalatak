package chess
package format.pgn
import scala.collection.mutable.Stack
object Dumper {

  def apply(situation: Situation, data: chess.Move, next: Situation): String = {
    import data._

    ((piece.role) match {
      case (role) =>
        // Check whether there is a need to disambiguate:
        //   - can a piece of same role move to/capture on the same square?
        //   - if so, disambiguate, in order or preference, by:
        //       - file
        //       - rank
        //       - both (only happens w/ at least 3 pieces of the same role)
        val candidates = situation.board.pieces collect {
          case (cpos, Stack(cpiece, _*)) if cpiece == piece && cpos != orig && cpiece.eyes(cpos, dest) => cpos
        } filter { cpos =>
          // We know Role ≠ Pawn, so it is fine to always pass None as promotion target
          situation.move(cpos, dest, 0).isValid
        }

        val disambiguation = if (candidates.isEmpty) {
          ""
        } else if (!candidates.exists(_ ?| orig)) {
          orig.file.toString
        } else if (!candidates.exists(_ ?- orig)) {
          orig.rank.toString
        } else {
          orig.key
        }

        s"${role.pgn}$disambiguation${if (captures) "x" else ""}${dest.key}"
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
