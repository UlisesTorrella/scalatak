import chess._
import scala.collection.mutable.Stack
import cats.data.Validated.Valid

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
      move <- g1.situation.move(1, Pos.E4, Direction.Up, List(1))
      g2 = g1(move)
      u3 = println(s"Moved $move")
      drop3 <- variant.Crazyhouse.drop(g2.situation, Flatstone, Pos.A1)
      g3 = g2.applyDrop(drop3)
      u4 = println(s"Droped $drop3")
      move1 <- g3.situation.move(1, Pos.E5, Direction.Left, List(1))
      u2 = println(s"Turn of ${g3.situation.color} $move1")
    } yield g3(move1)

    for {
      g <- gameStack
    } yield g.situation.board
  }

  def test2 = {
    val game = Game(variant.Crazyhouse)

    val piece1 = Piece(Color.White, Flatstone)
    val gameStack = for {
      drop <- variant.Crazyhouse.drop(game.situation, Flatstone, Pos.E4)
      g = game.applyDrop(drop)
      u = println(s"Droped $drop")
      drop1 <- variant.Crazyhouse.drop(g.situation, Flatstone, Pos.E5)
      g1 = g.applyDrop(drop1)
      u1 = println(s"Droped $drop1")
      move <- g1.situation.move(1, Pos.E4, Direction.Up, List(1))
      g2 = g1(move)
      u3 = println(s"Moved $move")
      drop3 <- variant.Crazyhouse.drop(g2.situation, Flatstone, Pos.E6)
      g3 = g2.applyDrop(drop3)
      u4 = println(s"Droped $drop3")
      move1 <- g3.situation.move(2, Pos.E5, Direction.Left, List(1,1))
      u2 = println(s"Turn of ${g3.situation.color} $move1")
    } yield g3(move1)

    for {
      g <- gameStack
    } yield g.situation.board
  }

  def test3 = {
    val wp = Piece(Color.White, Flatstone)
    val bp = Piece(Color.Black, Flatstone)
    val game = Game(Board(Map(Pos.E5 -> Stack(wp,bp,wp,bp,wp,bp,wp,bp)), variant.Standard))

    val piece1 = Piece(Color.White, Flatstone)
    val gameStack = for {
      move1 <- game.situation.move(5, Pos.E5, Direction.Left, List(3,1,1))
      u2 = println(s"Turn of ${game.situation.color} $move1")
    } yield game(move1)

    for {
      g <- gameStack
    } yield g.situation.board
  }


  val a = test3 getOrElse (Board(Map(), chess.variant.Standard))

}
