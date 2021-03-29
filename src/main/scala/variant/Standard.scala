package chess
package variant

import scala.collection.mutable.Stack

case object Standard
    extends Variant(
      id = 1,
      key = "standard",
      name = "Standard",
      shortName = "Std",
      title = "Standard rules of chess (FIDE)",
      standardInitialPosition = true
    ) {

  val pieces: Map[Pos, Stack[Piece]] = Map.empty[Pos, Stack[Piece]]
}
