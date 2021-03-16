package chess
package variant

case object Tak
    extends Variant(
      id = 1,
      key = "tak",
      name = "Tak",
      shortName = "tak",
      title = "Tak",
      standardInitialPosition = false
    ) {

  val pieces: Map[Pos, Piece] = Map.empty[Pos, Piece]
}
