package chess
package format.pgn

import scala.util.Try

object Binary {

  def writeMove(m: String)             = Try(Writer move m)
  def writeMoves(ms: Iterable[String]) = Try(Writer moves ms)

  def readMoves(bs: List[Byte])          = Try(Reader moves bs)
  def readMoves(bs: List[Byte], nb: Int) = Try(Reader.moves(bs, nb))

  private object MoveType {
    val SimplePawn  = 0
    val SimplePiece = 1
    val Drop        = 2
    val FullPiece   = 3
  }

  private object Encoding {
    val pieceInts: Map[String, Int] =
      Map("F" -> 1, "C" -> 2, "W" -> 3)
    val pieceStrs: Map[Int, String]     = pieceInts map { case (k, v) => v -> k }
    val dropPieceInts: Map[String, Int] = Map("F" -> 1, "C" -> 2, "W" -> 3)
    val dropPieceStrs: Map[Int, String] = dropPieceInts map { case (k, v) => v -> k }
    val promotionInts: Map[String, Int] = Map("" -> 0, "F" -> 1, "C" -> 2, "W" -> 3)
    val promotionStrs: Map[Int, String] = promotionInts map { case (k, v) => v -> k }
    val checkInts: Map[String, Int]     = Map("" -> 0, "+" -> 1, "#" -> 2)
    val checkStrs: Map[Int, String]     = checkInts map { case (k, v) => v -> k }
    val dirInts: Map[String, Int]       = Map(">" -> 0, "<" -> 1, "+" -> 2, "-" -> 3)
    val dirStrs: Map[Int, String]       = dirInts map { case (k, v) => v -> k }
  }

  private object Reader {

    import Encoding._

    private val maxPlies = 600

    def moves(bs: List[Byte]): List[String]          = moves(bs, maxPlies)
    def moves(bs: List[Byte], nb: Int): List[String] = intMoves(bs map toInt, nb)

    def intMoves(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0 => Nil
        case Nil                 => Nil
        case b1 :: b2 :: rest if moveType(b1) == MoveType.Drop =>
          drop(b1, b2) :: intMoves(rest, pliesToGo - 1)
        case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: rest if moveType(b1) == MoveType.SimplePiece =>
          simplePiece(b1, b2, b3, b4, b5, b6) :: intMoves(rest, pliesToGo - 1)
        case x => !!(x map showByte mkString ",")
      }

    def simplePawn(i: Int): String = posString(right(i, 6))

    // 2 movetype
    // 6 pos
    // ----
    // 3 role
    // 2 check
    // 1 capture
    // 1 drop
    // 1 nothing
    def simplePiece(b1: Int, b2: Int, b3: Int, b4: Int, b5: Int, b6: Int): String = {
        val index = (b2 >>> 5)
        val pos   = posString(right(b1, 6))
        val check = checkStrs(cut(b2, 5, 3))
        val dir   = dirStrs(cut(b2, 3, 1))
        val dropsList = List(b3>>>4, right(b3, 4), b4>>>4, right(b4, 4), b5>>>4, right(b5, 4), b6>>>4, right(b6, 4), 0)
        val drops = dropsList.splitAt(dropsList.indexOf(0))._1
                      .mkString("")
        s"$index$pos$dir$drops$check"
      }

    def drop(b1: Int, b2: Int): String = {
      val piece = dropPieceStrs(b2 >> 5)
      val pos   = posString(right(b1, 6))
      val check = checkStrs(cut(b2, 5, 3))
      s"$piece@$pos$check"
    }

    def fullPawn(b1: Int, b2: Int): String = {
      val pos = posString(right(b1, 6))
      val fileCapture = (b2 >> 6) match {
        case 1 => s"${(pos(0) - 1).toChar}x"
        case 2 => s"${(pos(0) + 1).toChar}x"
        case _ => ""
      }
      val check     = checkStrs(cut(b2, 6, 4))
      val prom      = promotionStrs(cut(b2, 4, 1))
      val promotion = if (prom.isEmpty) "" else s"=$prom"
      s"$fileCapture$pos$promotion$check"
    }

    def fullPiece(b1: Int, b2: Int, b3: Int): String = {
      val pos     = posString(right(b1, 6))
      val piece   = pieceStrs(b2 >> 5)
      val capture = if (bitAt(b2, 3)) "x" else ""
      val check   = checkStrs(cut(b2, 5, 3))
      val disamb = (b3 >> 6) match {
        case 0 => fileChar(right(b3, 3)).toString
        case 1 => rankChar(right(b3, 3)).toString
        case _ => posString(right(b3, 6))
      }
      s"$piece$disamb$capture$pos$check"
    }

    private def moveType(i: Int)  = i >> 6
    private def posString(i: Int) = fileChar(i >> 3).toString + rankChar(right(i, 3))
    private def fileChar(i: Int)  = (i + 97).toChar
    private def rankChar(i: Int)  = (i + 49).toChar

    private def right(i: Int, x: Int): Int           = i & lengthMasks(x)
    private def cut(i: Int, from: Int, to: Int): Int = right(i, from) >> to
    private def bitAt(i: Int, p: Int): Boolean       = cut(i, p, p - 1) != 0
    private val lengthMasks =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)
    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    import Encoding._

    def move(str: String): List[Byte] =
      (str match {
        case SimplePieceR(index, pos, dir, drops, check) =>
          simplePiece(index, pos, dir, drops, check)
        case DropR(role, pos, check) => drop(role, pos, check)
      }) map (_.toByte)

    def moves(strs: Iterable[String]): Array[Byte] = strs.flatMap(move).to(Array)

    def dropAt(i: Int, dropS: String): Int =
      dropS lift i match {
        case Some(x) => x.toInt - 48
        case None    => 0
      }

    def simplePiece(index: String, pos: String, dir: String, drops: String, check: String) =
      List(
        (MoveType.SimplePiece << 6) + posInt(pos),
        (index.toInt << 5) + (checkInts(check) << 3) + (dirInts(dir) << 1),
        (dropAt(0, drops) << 4) + (dropAt(1, drops)),
        (dropAt(2, drops) << 4) + (dropAt(3, drops)),
        (dropAt(4, drops) << 4) + (dropAt(5, drops)),
        (dropAt(6, drops) << 4) + (dropAt(7, drops))
      )

    def drop(piece: String, pos: String, check: String) =
      List(
        (MoveType.Drop << 6) + posInt(pos),
        (dropPieceInts(piece) << 5) + (checkInts(check) << 3) + (1 << 1)
      )

    def disambTypeInt(orig: String): Int =
      if (orig.length > 1) 2
      else if (orig.head.toInt < 97) 1
      else 0

    def disambiguationInt(orig: String): Int =
      if (orig.length > 1) posInt(orig)
      else if (orig.head.toInt < 97) rankInt(orig.head)
      else fileInt(orig.head)

    def boolInt(s: String): Int  = if (s.nonEmpty) 1 else 0
    def boolInt(b: Boolean): Int = if (b) 1 else 0

    def posInt(pos: String): Int    = posInt(fileInt(pos.head), rankInt(pos(1)))
    def posInt(x: Int, y: Int): Int = (x << 3) + y

    def fileInt(c: Char): Int = c.toInt - 97
    def rankInt(c: Char): Int = c.toInt - 49

    def shiftOptionInt(fileOption: Option[String], pos: String): Int =
      fileOption.fold(0) { file =>
        if (file.head < pos.head) 1 else 2
      }

    val pieceR       = "([FCW])"
    val fileR        = "(?:([a-h])x)?"
    val posR         = "([a-h][1-9])"
    val captureR     = "(x?)"
    val checkR       = "([\\+#]?)"
    val promotionR   = "(?:\\=?([FCW]))?"
    val indexR       = "([0-9]?)"
    val dirR         = "([<>+-])"
    val dropsR       = """(\d+)"""
    val origR        = "([a-h]?[1-8]?)".r
    val SimplePieceR = s"^$indexR$posR$dirR$dropsR$checkR$$".r
    val FullPawnR    = s"^$fileR$posR$promotionR$checkR$$".r
    val CastlingR    = s"^(O-O|O-O-O)$checkR$$".r
    val FullPieceR   = s"^$pieceR$origR$captureR$posR$checkR$$".r
    val DropR        = s"^([FCW])@$posR$checkR$$".r
  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
