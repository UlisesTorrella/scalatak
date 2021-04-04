import chess.format.pgn.Binary

object binaryPgnTest {
  val one = Binary.writeMove("1e4>1")
  def test1 = "1e4>1"==Binary.readMoves(one get)
  val two = Binary.writeMove("2e4>11")
  def test2 = "2e4>11"==Binary.readMoves(two get)
  val three = Binary.writeMove("4e4>112")
  def test3 = "4e4>112"==Binary.readMoves(three get)
  val four = Binary.writeMove("4e4>112")
  def test4 = "4e4>112"==Binary.readMoves(four get)
}
