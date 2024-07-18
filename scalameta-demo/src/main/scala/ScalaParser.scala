import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment

case class ScaladocInfo(description: String, params: List[String], tparams: List[String])

object ScalaParser {

  def parseAndExtractInfo(source: String): Map[String, ScaladocInfo] = {
    val parsed: Source = source.parse[Source].get

    val comments = parsed.tokens.collect {
      case comment: Comment if comment.syntax.startsWith("/**") =>
        (comment.pos.startLine, ScaladocParser.parseScaladoc(comment).getOrElse(Nil))
    }

    val functions = parsed.collect {
      case defn @ Defn.Def(_, name, _, _, _, _) => (name.value, defn.pos.startLine)
    }

    var scaladocInfoMap = Map[String, ScaladocInfo]()

    var lastComment = -1
    functions.foreach { case (functionName, functionLine) =>
      while (lastComment + 1 < comments.length && comments(lastComment + 1)._1 < functionLine) {
        lastComment += 1
      }
      if (lastComment >= 0) {
        val tokens = comments(lastComment)._2
        
        val descriptions = tokens.collect {
          case DocToken(Description,name, body) => body
        }.mkString(" ")

        val params = tokens.collect {
          case DocToken(Param, Some(name), desc) => s"@$name: ${desc.getOrElse("")}"
        }

        val tparams = tokens.collect {
          case DocToken(TypeParam, Some(name), desc) => s"@tparam $name: ${desc.getOrElse("")}"
        }

        scaladocInfoMap += (functionName -> ScaladocInfo(descriptions, params, tparams))
      } else {
        scaladocInfoMap += (functionName -> ScaladocInfo("", Nil, Nil))
      }
    }

    scaladocInfoMap
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
