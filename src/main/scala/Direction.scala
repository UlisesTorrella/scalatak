package chess

object Direction extends Enumeration {
    type Direction = Value
    val Up = Value("+")
    val Down = Value("-")
    val Left = Value("<")
    val Right = Value(">")
    //junior, expert and senior are just to pass the test

    val allDirs = Map("+" -> Up,
                      "-" -> Down,
                      "<" -> Left,
                      ">" -> Right)

    def toChar(dir: Direction) =
      dir match {
        case Left  => '<'
        case Right => '>'
        case Up    => '+'
        case Down  => '-'
      }

    def toInt(s: String) =
      s match {
        case ">" => 0
        case "<" => 1
        case "+" => 2
        case "-" => 3
        case _   => 3
      }

    def apply(s: String) =
      s match {
        case ">" => Right
        case "<" => Left
        case "+" => Up
        case "-" => Down
        case _   => Up
      }

    def apply(s: Char): Direction = Direction(s"$s")

    def apply(dir: Direction, p: Pos): Option[Pos] =
      dir match {
        case Left  => Pos.at(p.file.index - 1, p.rank.index)
        case Right => Pos.at(p.file.index + 1, p.rank.index)
        case Up    => Pos.at(p.file.index, p.rank.index + 1)
        case Down  => Pos.at(p.file.index, p.rank.index - 1)
      }
}
