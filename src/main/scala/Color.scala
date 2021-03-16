package chess

sealed trait Color {

  final def -(role: Role) = Piece(this, role)

  final def fold[A](w: => A, b: => A): A = if (white) w else b

  def unary_! : Color

  val passablePawnRank: Rank
  val promotablePawnRank: Rank
  val backRank: Rank

  val letter: Char
  val name: String

  final def pawn   = this - Pawn
  final def bishop = this - Bishop
  final def knight = this - Knight
  final def rook   = this - Rook
  final def queen  = this - Queen
  final def king   = this - King

  final val white = this == Color.White
  final val black = this == Color.Black
}

object Color {

  case class Map[A](white: A, black: A) {
    def apply(color: Color) = if (color.white) white else black

    def update(color: Color, f: A => A) = {
      if (color.white) copy(white = f(white))
      else copy(black = f(black))
    }

    def map[B](fw: A => B, fb: A => B) = copy(white = fw(white), black = fb(black))

    def map[B](f: A => B): Map[B] = map(f, f)

    def all: Seq[A] = Seq(white, black)

    def reduce[B](f: (A, A) => B) = f(white, black)

    def forall(pred: A => Boolean) = pred(white) && pred(black)

    def exists(pred: A => Boolean) = pred(white) || pred(black)
  }

  object Map {
    def apply[A](f: Color => A): Map[A] = Map(white = f(White), black = f(Black))
  }

  case object White extends Color {

    def unary_! = Black

    val passablePawnRank   = Rank.Fifth
    val promotablePawnRank = Rank.Eighth
    val backRank           = Rank.First

    val letter = 'w'
    val name   = "white"

    override val hashCode = 1
  }

  case object Black extends Color {

    def unary_! = White

    val passablePawnRank   = Rank.Fourth
    val promotablePawnRank = Rank.First
    val backRank           = Rank.Eighth

    val letter = 'b'
    val name   = "black"

    override val hashCode = 2
  }

  def fromPly(ply: Int) = fromWhite((ply & 1) == 0)

  def fromWhite(white: Boolean): Color = if (white) White else Black

  def fromName(n: String): Option[Color] =
    if (n == "white") Option(White)
    else if (n == "black") Option(Black)
    else None

  def apply(c: Char): Option[Color] =
    if (c == 'w') Option(White)
    else if (c == 'b') Option(Black)
    else None

  val white: Color = White
  val black: Color = Black

  val all = List(White, Black)

  def showResult(color: Option[Color]) =
    color match {
      case Some(chess.White) => "1-0"
      case Some(chess.Black) => "0-1"
      case None              => "1/2-1/2"
    }

  def fromResult(result: String): Option[Color] =
    result match {
      case "1-0" => Option(chess.White)
      case "0-1" => Option(chess.Black)
      case _     => None
    }
}
