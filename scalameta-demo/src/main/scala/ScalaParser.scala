import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment

case class ScaladocInfo(description: String, params: List[String], tparams: List[String])

object ScalaParser {

  def parseAndExtractInfo(source: String): Map[String, ScaladocInfo] = {
    val parsed: Source = source.parse[Source].get

    val allComments = parsed.tokens.collect {
      case comment: Comment if comment.syntax.startsWith("/**") =>
        (comment.pos.start, ScaladocParser.parseScaladoc(comment).getOrElse(Nil))
    }.toMap

    val functions = parsed.collect {
      case defn @ Defn.Def(_, name, _, _, _, _) =>
        val comments = allComments
          .filter { case (start, _) => start < defn.pos.start }
          .maxByOption(_._1)
          .map(_._2)
          .getOrElse(Nil)

        val description = comments.collect {
          case DocToken(Description, _, body) => body
        }.mkString(" ")

        val params = comments.collect {
          case DocToken(Param, Some(name), desc) => s"@$name: ${desc.getOrElse("")}"
        }

        val tparams = comments.collect {
          case DocToken(TypeParam, Some(name), desc) => s"@tparam $name: ${desc.getOrElse("")}"
        }

        name.value -> ScaladocInfo(description, params, tparams)
    }.toMap

    functions
  }
}

object HelloWorld extends App {
  println("Welcome to Standard Scaladoc Parser")

  val source = """
    /** This is a Scaladoc comment
      * This is a description for the object Main.
      */
    object Main {
      /** 
        * This function sums two integers.
        * @param a The first parameter
        * @param b The second parameter
        * @tparam T The type parameter
        */
      def sum[T](a: Int, b: Int): Int = a + b

      /** 
        * This function greets the user.
        * @param name The name parameter
        */
      def greet(name: String): Unit = println(s"Hello, $name!")

      /** 
        * This function subtracts two integers.
        * @param c The first parameter to subtract
        * @param d The second parameter to subtract
        * @tparam T The type parameters
        */
      def subtract[T](c: Int, d: Int): Int = c - d
    }
  """

  val scaladocInfoMap = ScalaParser.parseAndExtractInfo(source)

  scaladocInfoMap.foreach { case (functionName, info) =>
    println(s"Function: $functionName")
    println(s"  Description: ${info.description}")
    println(s"  Params: ${info.params.mkString(", ")}")
    println(s"  Type Params: ${info.tparams.mkString(", ")}")
  }
}
