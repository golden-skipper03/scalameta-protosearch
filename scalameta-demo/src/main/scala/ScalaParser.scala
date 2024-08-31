import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment
import scala.util.matching.Regex
// Case class to hold function information along with its Scaladoc
case class EnhancedDefnDef(
  name: String,
  params: List[String],
  tparams: List[String],
  description: String,
    annotations: List[String],
  hyperlinks: List[String],
  originalDefn: Defn.Def
)

object ScalaParser {

  def parseAndExtractInfo(source: String): List[EnhancedDefnDef] = {
    val parsed: Source = source.parse[Source].get

    // Regular expression to match hyperlinks in the format [text](url)
    val hyperlinkRegex: Regex = """\[(.*?)\]\((.*?)\)""".r

    // Extract comments and their positions
    val comments = parsed.tokens.collect {
      case comment: Comment if comment.syntax.startsWith("/**") =>
        (comment.pos.start, ScaladocParser.parseScaladoc(comment).getOrElse(Nil))
    }.toMap

    // Traverse the AST to find functions and their associated comments
    val functions = parsed.collect {
      case defn @ Defn.Def(_, name, _, _, _, _) =>
    // Find the closest preceding comment
val (commentTokens, rawComment): (List[DocToken], String) = comments
  .filter { case (start, _) => start < defn.pos.start }
  .maxByOption(_._1)
  .map { case (_, tokens) =>
    val raw = tokens.collect {
      case DocToken(_, _, Some(body)) => body
    }.mkString(" ")
    (tokens, raw)
  }
  .getOrElse((Nil, ""))

  //.getOrElse(Nil,"")

        val description = commentTokens.collect {
          case DocToken(Description, _, Some(body)) => body
        }.mkString(" ")

        val params = commentTokens.collect {
          case DocToken(Param, Some(name), Some(desc)) => s"@$name: $desc"
        }

        val tparams = commentTokens.collect {
          case DocToken(TypeParam, Some(name), Some(desc)) => s"@tparam $name: $desc"
        }

        // Extract annotations associated with the function
        val annotations = defn.mods.collect {
          case mod: Mod.Annot => mod.toString
        } 
        // Extract hyperlinks from the raw Scaladoc comment using regex
        val hyperlinks = hyperlinkRegex.findAllMatchIn(rawComment).map { m =>
          s"[${m.group(1)}](${m.group(2)})"
        }.toList
        EnhancedDefnDef(name.value, params, tparams, description, annotations, hyperlinks, defn)
    }

    functions
  }
}

object HelloWorld extends App {
  println("Welcome to Enhanced Scaladoc Parser")

  

  
  val fileName = "E:/Gsoc/scalameta-demo/scalameta-demo/src/main/scala/data.scala"
  val bufferedSource = scala.io.Source.fromFile(fileName)
  //val text = bufferedSource.getLines().mkString
  val text = bufferedSource.mkString
  bufferedSource.close()
  // println(s"File content:\n$text")

  val scaladocInfoList = ScalaParser.parseAndExtractInfo(text)

  scaladocInfoList.foreach { info =>
    println(s"Function: ${info.name}")
    println(s"  Description: ${info.description}")
    println(s"  Params: ${info.params.mkString(", ")}")
    println(s"  Type Params: ${info.tparams.mkString(", ")}")
    println(s"  Annotations: ${info.annotations.mkString(", ")}")
    println(s"  Hyperlinks: ${info.hyperlinks.mkString(", ")}")
  }

}

