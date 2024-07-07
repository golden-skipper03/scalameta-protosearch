import scala.meta._

object ScalaParser extends App {

  val source = """
    // This is a comment
    object Main extends App {
      def sum(a: Int, b: Int): Int = a + b
    
      def greet(name: String): Unit = println(s"Hello, $name!")
    } 
    """

  // Parse the source into a Source object
  val tree = source.parse[Source].get

  // Extract comments from the source string
  val comments = tree.tokens.collect {
    case Token.Comment(value) => value.trim
  }

  // Extract function definitions from the source tree
  val functions = tree.collect {
    case q"..$mods def $fname(..$params): $returnType = $_" =>
      val paramList = params.map {
        case param"..$mods $name: $tpe" => s"${name.syntax}: ${tpe.toString}"
      }
      (fname.syntax, paramList)
  }

  // Print extracted comments
  println("Comments:")
  comments.foreach(println)

  // Print extracted function names and parameters
  println("\nFunctions:")
  functions.foreach { case (fname, params) =>
    println(s"Function: $fname")
    println(s"Parameters: ${params.mkString(", ")}")
    println()
  }
}
