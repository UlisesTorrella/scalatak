package chess

import chess.format.pgn.Binary
import org.specs2.mutable.Specification
import scala.util.{Success}

class BinaryPgnTest extends Specification {
  val one = Binary.writeMove("1e4>1")

  "1e4>1" should {
    "read from Binary" in {
      Success(List("1e4>1"))==Binary.readMoves(one get)
    }
  }

  //def test1 = "1e4>1"==Binary.readMoves(one get)
  val two = Binary.writeMove("2e4>11")

  "2e4>11" should {
    "read from Binary" in {
      Success(List("2e4>11"))==Binary.readMoves(two get)
    }
  }

  val three = Binary.writeMove("4e4>112")

  "4e4>112" should {
    "read from Binary" in {
      Success(List("4e4>112"))==Binary.readMoves(three get)
    }
  }

  val four = Binary.writeMove("4e4>22")

  "4e4>22" should {
    "read from Binary" in {
      Success(List("4e4>22"))==Binary.readMoves(four get)
    }
  }
}
