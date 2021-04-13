package chess


import org.specs2.mutable.Specification
import scala.collection.mutable.Stack
import cats.data.Validated.Valid

class GameTest extends Specification {

  "simple drop and move " should {
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

    val b = (for {
      g <- gameStack
    } yield g.situation.board) getOrElse (Board(Map(), chess.variant.Standard))

    "have pieces in" in {
      b.pieces.get(Pos.E5).nonEmpty
      b.pieces.get(Pos.A1).nonEmpty
      b.pieces.get(Pos.D5).nonEmpty
    }
  }

  "simple long drop" should {
    val game = Game(variant.Crazyhouse)

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

    val b = (for {
      g <- gameStack
    } yield g.situation.board) getOrElse (Board(Map(), chess.variant.Standard))


    "have pieces in" in {
      b.pieces.get(Pos.D5).nonEmpty
      b.pieces.get(Pos.C5).nonEmpty
      b.pieces.get(Pos.E6).nonEmpty
    }
  }

  "long drop 3 1 1" should {
    val wp = Piece(Color.White, Flatstone)
    val bp = Piece(Color.Black, Flatstone)
    val game = Game(Board(Map(Pos.E5 -> Stack(wp,bp,wp,bp,wp,bp,wp,bp)), variant.Standard))

    val gameStack = for {
      move1 <- game.situation.move(5, Pos.E5, Direction.Left, List(3,1,1))
      u2 = println(s"Turn of ${game.situation.color} $move1")
    } yield game(move1)

    val b = (for {
      g <- gameStack
    } yield g.situation.board) getOrElse (Board(Map(), chess.variant.Standard))

    "have pieces in" in {
      b.pieces.get(Pos.E5).nonEmpty
      b.pieces.get(Pos.D5).size == 3
      b.pieces.get(Pos.C5).nonEmpty
      b.pieces.get(Pos.B5).nonEmpty
    }
  }

  "capstone over Wallstone " should {
    val wc = Piece(Color.White, Capstone)
    val bw = Piece(Color.Black, Wallstone)
    val game = Game(Board(Map(Pos.E5 -> Stack(wc), Pos.D5 -> Stack(bw)), variant.Standard))

    val gameStack = for {
      move1 <- game.situation.move(1, Pos.E5, Direction.Left, List(1))
      u2 = println(s"Turn of ${game.situation.color} $move1")
    } yield game(move1)

    val b = (for {
      g <- gameStack
    } yield g.situation.board) getOrElse (Board(Map(), chess.variant.Standard))

    "have cap over flat" in {
      (b.pieces.get(Pos.D5) getOrElse Stack()).top is Capstone
      (b.pieces.get(Pos.D5) getOrElse Stack())(1) is Flatstone
    }
  }
}
