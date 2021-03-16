package chess
package variant

case object Standard
    extends Variant(
      id = 1,
      key = "standard",
      name = "Standard",
      shortName = "Std",
      title = "Standard rules of chess (FIDE)",
      standardInitialPosition = false
    ) {

  val pieces: Map[Pos, Piece] = Map.empty[Pos, Piece]
}
