import chess._
import scala.collection.mutable.Stack

object gameTest {

  def test1 = {
    val game = Game(variant.Crazyhouse)

    val piece1 = Piece(Color.White, Flatstone)
    val gameStack = for {
      drop <- variant.Crazyhouse.drop(game.situation, Flatstone, Pos.E4)
      g = game.applyDrop(drop)
      u = println(s"Droped $drop")
      drop1 <- variant.Crazyhouse.drop(g.situation, Flatstone, Pos.E5)
      g1 = g.applyDrop(drop1)
      u1 = println(s"Droped $drop1")
      move <- g1.situation.move(Pos.E4, Pos.E5, 0)
      g2 = g1(move)
      u3 = println(s"Moved $move")
      drop3 <- variant.Crazyhouse.drop(g2.situation, Flatstone, Pos.A1)
      g3 = g2.applyDrop(drop3)
      u4 = println(s"Droped $drop3")
      move1 <- g3.situation.move(Pos.E5, Pos.D5, 0)
      u2 = println(s"Turn of ${g3.situation.color} $move1")
    } yield g3(move1)

    for {
      g <- gameStack
    } yield g.situation.board
  }

}
