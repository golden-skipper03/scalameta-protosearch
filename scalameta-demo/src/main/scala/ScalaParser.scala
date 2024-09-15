import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.DocToken._
import scala.meta.tokens.Token.Comment
import scala.util.matching.Regex

case class EnhancedDefnDef(
  name: String,
  description: String,
  annotations: List[String],
  hyperlinks: List[String],
  params  : List[String],
  returnType: String,
  originalDefn: Defn.Def,
  functionSource: String,
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

    val sortedComments = comments.toList.sortBy(_._1)

    val firstComment = sortedComments.headOption

    val FilePathComment = firstComment
      .map { case (start, commentTokens) =>
        commentTokens.collect {
          case DocToken(_, _, Some(body)) => body
        }.mkString(" ")
      }
      .getOrElse("No comments found.")

    val url="https://github.com/"+FilePathComment+"#L"

    val functions = parsed.collect {
      case defn @ Defn.Def(mods, name, tparams, paramss, retType, _) =>
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

        
      val startLine = defn.pos.startLine;      
      val endLine = defn.pos.endLine;  

      val description = commentTokens.collect {
        case DocToken(Description, _, Some(body)) => body
      }.mkString(" ")

      val paramsComm  = commentTokens.collect {
        case DocToken(Param, Some(name), Some(desc)) => s"$name: $desc"
      }

      val params = paramss.flatten.map { param =>
        s"${paramsComm.find(_.startsWith(""+param.name.value)).getOrElse(param.name.value)}: ${param.decltpe.map(_.toString).getOrElse("Unknown Type")} " 
      }

      val typeParamsComm= commentTokens.collect {
        case DocToken(TypeParam, Some(name), Some(desc)) => s"@tparam $name: $desc"
      }

      val typeParams = tparams.zipWithIndex.map { case (tparam, index)    =>
        val value = typeParamsComm.lift(index).getOrElse(tparam.name.value)
        s"${value.replace("@tparam", "")}"
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
     
      val allParams = optionalParams ++ typeParams ++ implicitParams 

      val returnType = retType match {
        case Some(tpe) => tpe.syntax
        case None => "Unit"
      }

      EnhancedDefnDef(
        name.value,
        description , 
        annotations,
        hyperlinks ,
        allParams,
        returnType,
        originalDefn = defn,
        url+startLine
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
    println(s"  Annotations: ${info.annotations.mkString(", ")}")
    println(s"  Hyperlinks: ${info.hyperlinks.mkString(", ")}")
    println(s"  params: ${info.params.mkString(", ")}")
    println(s"  Return Type: ${info.returnType}")
    println(s"  url: ${info.functionSource}")
  }
}
