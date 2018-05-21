package typetoy

import scala.annotation.tailrec
import scala.io._

object TypeToy extends App {

  @tailrec
  def main() : Unit = {
    var input = StdIn.readLine(">>")

    if (input.isEmpty)
      ()
    else {

      def result =
        for {
          tkns <- Lexer.apply(input).right
          tree <- Parser.apply(tkns).right
          equs <- Typer.apply(tree).right
          ty <- Solver.apply(equs).right
        } yield TypeFunctions.print(ty)

      println(result)
      main()
    }
  }

  main()
}
