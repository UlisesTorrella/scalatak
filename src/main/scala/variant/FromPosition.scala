package chess
package variant

case object FromPosition
    extends Variant(
      id = 3,
      key = "fromPosition",
      name = "From Position",
      shortName = "FEN",
      title = "Custom starting position",
      standardInitialPosition = false
    ) {

  def pieces = Standard.pieces

  override def canDropStuff(situation: Situation): Option[List[Pos]] =
    situation.board.crazyData match {
      case Some(data) => if (data.pockets(situation.color).roles.nonEmpty) Some(situation.board.emptySquares) else None
      case None => None
    }
}
