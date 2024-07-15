import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment

object ScalaParser {

  def parseAndExtractInfo(source: String): (List[String],  List[String], List[String], List[String]) = {
    val parsed: Source = source.parse[Source].get

    val comments = parsed.tokens.collect {
      case comment: Comment if comment.syntax.startsWith("/**") =>
        ScaladocParser.parseScaladoc(comment).getOrElse(Nil)
    }.flatten.toList

    val descriptions = comments.collect {
      case DocToken(Description, content, _) => content
    }.flatten.toList

    val params = comments.collect {
      case DocToken(Param, name, desc) => s"@$name: $desc"
    }.toList


    val functions = parsed.collect {
      case Defn.Def(_, name, _, _, _, _) => name.value
    }.toList

    val classes = parsed.collect {
      case Defn.Class(_, name, _, _, _) => name.value
    }.toList

    (descriptions, params,  functions, classes)
  }
}

object HelloWorld extends App {
  println("Welcome to Standard Scaladoc Parser")

  val source = """
    /** This is a Scaladoc comment
      * @param a The first parameter
      * @param b The second parameter
      */

      class sample{}
    object Main {
      def sum[T](a: Int, b: Int): Int = a + b
    }
  """

  val (descriptions, params,  functions, classes) = ScalaParser.parseAndExtractInfo(source)

  println(s"Descriptions: ${descriptions.mkString("\n")}")
  println(s"Params: ${params.mkString("\n")}")
  println(s"Functions: ${functions.mkString(", ")}")
  println(s"Classes: ${classes.mkString(", ")}")
}
