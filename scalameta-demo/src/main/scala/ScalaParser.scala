import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment
import scala.util.matching.Regex

case class EnhancedDefnDef(
  name: String,
  params: List[String],
  tparams: List[String],
  description: String,
  annotations: List[String],
  hyperlinks: List[String],
  optionalParams: List[String],
  implicitParams: List[String],
  originalDefn: Defn.Def
)

object ScalaParser {

  def parseAndExtractInfo(source: String): List[EnhancedDefnDef] = {
    val parsed: Source = source.parse[Source].get

    val urlRegex: Regex = """https?://[^\s]+""".r

    // Extract comments and their positions
    val comments = parsed.tokens.collect {
      case comment: Comment if comment.syntax.startsWith("/**") =>
        (comment.pos.start, ScaladocParser.parseScaladoc(comment).getOrElse(Nil))
    }.toMap

    val functions = parsed.collect {
      case defn @ Defn.Def(mods, name, tparams, paramss, _, _) =>
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

      val description = commentTokens.collect {
        case DocToken(Description, _, Some(body)) => body
      }.mkString(" ")

      val typeParams = tparams.map { tparam =>
        tparam.name.value
      }

      val params = paramss.flatten.map { param =>
        s"${param.name.value}: ${param.decltpe.map(_.toString).getOrElse("Unknown Type")}"
      }

      val annotations = defn.mods.collect {
        case mod: Mod.Annot => mod.toString
      }

      val hyperlinks = urlRegex.findAllMatchIn(rawComment).map(_.matched).toList

      val optionalParams = paramss.flatten.collect {
        case param: Term.Param if param.default.isDefined => param.name.value
      }

      val implicitParams = paramss.collect {
        case params if params.exists(_.mods.exists(_.is[Mod.Implicit])) =>
          params.collect {
            case param: Term.Param => param.name.value
          }
      }.flatten

      EnhancedDefnDef(
        name.value,
        params,
        typeParams,
        description , 
        annotations,
        hyperlinks , 
        optionalParams,
        implicitParams,
        originalDefn = defn
      )
    }

    functions
  }
}

object HelloWorld extends App {
  println("Welcome to Enhanced Scaladoc Parser")

  val fileName = "E:/Gsoc/scalameta-demo/scalameta-demo/src/main/scala/data2.txt"
  val bufferedSource = scala.io.Source.fromFile(fileName)
  val text = bufferedSource.mkString
  bufferedSource.close()

  val scaladocInfoList = ScalaParser.parseAndExtractInfo(text)

  scaladocInfoList.foreach { info =>
    println(s"Function: ${info.name}")
    println(s"  Description: ${info.description}")
    println(s"  Params: ${info.params.mkString(", ")}")
    println(s"  Type Params: ${info.tparams.mkString(", ")}")
    println(s"  Annotations: ${info.annotations.mkString(", ")}")
    println(s"  Hyperlinks: ${info.hyperlinks.mkString(", ")}")
    println(s"  Optional Params: ${info.optionalParams.mkString(", ")}")
    println(s"  Implicit Params: ${info.implicitParams.mkString(", ")}")
  }
}
