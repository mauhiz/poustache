package net.mauhiz.poustache

import java.util.regex.Pattern

import net.mauhiz.poustache.MustacheAST._
import net.mauhiz.poustache.MustacheBlock.{InContext, InInverseContext, MustacheBlocks}
import org.parboiled2._

import scala.language.implicitConversions
import scala.util.matching.Regex

private class MustacheParser(val input: ParserInput, currentDelimiter: Delimiter)(implicit lineSeparator: LineSeparator) extends Parser with StringBuilding {

  import net.mauhiz.poustache.MustacheParser._

  // --- Rules ---
  private def contextName: Rule1[String] = rule {
    zeroOrMore(singleLineWhitespace) ~ clearSB() ~ oneOrMore(!currentDelimiter.close ~ validContextName ~ appendSB()) ~ push(sb.toString) ~ zeroOrMore(singleLineWhitespace)
  }

  private def partialName: Rule1[String] = rule {
    zeroOrMore(singleLineWhitespace) ~ clearSB() ~ oneOrMore(!currentDelimiter.close ~ validPartialName ~ appendSB()) ~ push(sb.toString) ~ zeroOrMore(singleLineWhitespace)
  }

  /**
   * Comment tags represent content that should never appear in the resulting output.
   * The tag's content may contain any substring (including newlines) EXCEPT the closing delimiter.
   * Comment tags SHOULD be treated as standalone when appropriate.
   */
  private def comment: Rule1[Comment] = rule {
    push(cursor) ~ currentDelimiter.open ~ str("!") ~ oneOrMore(!currentDelimiter.close ~ CharPredicate.All) ~ currentDelimiter.close ~ push(cursor) ~> Comment
  }

  private def contents: Rule1[Contents] = rule {
    push(cursor) ~ clearSB() ~ oneOrMore(!currentDelimiter.open ~ validContents ~ appendSB()) ~ push(sb.toString) ~ push(cursor) ~> Contents
  }

  private def partial: Rule1[Partial] = rule {
    push(cursor) ~ currentDelimiter.open ~ str(">") ~ partialName ~ currentDelimiter.close ~ push(cursor) ~> Partial
  }

  private def enterContext: Rule1[EnterContext] = rule {
    push(cursor) ~ currentDelimiter.open ~ str("#") ~ contextName ~ currentDelimiter.close ~ push(cursor) ~> EnterContext
  }

  private def enterNotContext: Rule1[EnterNotContext] = rule {
    push(cursor) ~ currentDelimiter.open ~ str("^") ~ contextName ~ currentDelimiter.close ~ push(cursor) ~> EnterNotContext
  }

  private def exitContext: Rule1[ExitContext] = rule {
    push(cursor) ~ currentDelimiter.open ~ str("/") ~ contextName ~ currentDelimiter.close ~ push(cursor) ~> ExitContext
  }

  /**
   * Set Delimiter tags are used to change the tag delimiters for all content following the tag in the current compilation unit.
   * The tag's content MUST be any two non-whitespace sequences (separated by whitespace) EXCEPT an equals sign ('=') followed by the current closing delimiter.
   * Set Delimiter tags SHOULD be treated as standalone when appropriate.
   */
  private def delimiterChange: Rule1[ChangeDelimiter] = rule {
    push(cursor) ~ currentDelimiter.open ~ str("=") ~ zeroOrMore(singleLineWhitespace) ~ capture(oneOrMore(validTag)) ~ oneOrMore(singleLineWhitespace) ~ capture(oneOrMore(validTag)) ~ zeroOrMore(singleLineWhitespace) ~ str("=") ~ currentDelimiter.close ~ push(cursor) ~> ChangeDelimiter
  }

  private def variable: Rule1[Variable] = rule {
    push(cursor) ~ currentDelimiter.open ~ contextName ~ currentDelimiter.close ~ push(cursor) ~> Variable
  }

  private def variableNoEscape1: Rule1[VariableNoEscape] = rule {
    push(cursor) ~ currentDelimiter.open ~ str("{") ~ contextName ~ str("}") ~ currentDelimiter.close ~ push(cursor) ~> VariableNoEscape
  }

  private def variableNoEscape2: Rule1[VariableNoEscape] = rule {
    push(cursor) ~ currentDelimiter.open ~ str("&") ~ contextName ~ currentDelimiter.close ~ push(cursor) ~> VariableNoEscape
  }

  // Block-level rules
  private def _block: Rule1[MustacheAST] = rule {
    enterContext | enterNotContext | exitContext | partial | comment | delimiterChange | contents | variableNoEscape1 | variableNoEscape2 | variable
  }

  def mustacheASTs: Rule1[Seq[MustacheAST]] = rule {
    zeroOrMore(_block) ~ EOI
  }

  // --- Predicates ---
  private val singleLineWhitespace = CharPredicate.from(Character.isWhitespace) -- lineSeparator.value
}

object MustacheParser {

  private def extractDelimChanges(s: String, from: Int = 0, delimiter: Delimiter = Delimiter.Default): Seq[(Int, Delimiter)] = {
    val startTag = s"${delimiter.open}="
    val start = s.indexOf(startTag, from)
    if (start < 0) {
      Seq(from -> delimiter)
    } else {
      val delimStart = start + startTag.size
      val closeTag = s"=${delimiter.close}"
      val delimEnd = s.indexOf(closeTag, delimStart)
      if (delimEnd < 0) {
        throw new TemplateParseException(start, "Unclosed delimiter change")
      } else {
        val newDelimiterStr = s.substring(delimStart, delimEnd)
        val newDelimiter = Delimiter.fromStr(delimStart, newDelimiterStr)
        val end = delimEnd + closeTag.size
        // FIXME make this tailrec
        (from -> delimiter) +: extractDelimChanges(s, end, newDelimiter)
      }
    }
  }

  private[poustache] def splitByDelim(s: String): Seq[(String, Delimiter)] = {
    val delimChanges: IndexedSeq[(Int, Delimiter)] = extractDelimChanges(s).toIndexedSeq
    for (i <- 0 until delimChanges.size) yield {
      val delimChange = delimChanges(i)
      val endPos = if (i < delimChanges.size - 1) {
        delimChanges(i + 1)._1
      } else {
        s.size
      }
      s.substring(delimChange._1, endPos) -> delimChange._2
    }
  }

  private[poustache] def handleStandalone(s: String, delim: Delimiter)(implicit lineSeparator: LineSeparator): String = {
    // comments, blocks, partials, change delim can be standalone
    // variables cannot
    val replaceStandalone = s"(?s)(\\A|${Pattern.quote(lineSeparator.value)})\\p{Blank}*(${Pattern.quote(delim.open)}\\s*[!#\\^>=/].*${Pattern.quote(delim.close)})\\p{Blank}*(\\z|${Pattern.quote(lineSeparator.value)})".r
    replaceStandalone.replaceAllIn(s, {
      mat =>
        val isBeginning = mat.group(1) != lineSeparator.value
        val tag = mat.group(2)
        val isEnd = mat.group(3) != lineSeparator.value
        if (isBeginning || isEnd) {
          tag
        } else {
          lineSeparator.value + tag
        }
    })
  }

  private[poustache] def parseWithDelim(woNlComments: String, delim : Delimiter)(implicit lineSeparator: LineSeparator): Seq[MustacheAST] = {
    val parser = new MustacheParser(woNlComments, delim)
    try {
      import org.parboiled2.Parser.DeliveryScheme.Throw
      parser.mustacheASTs.run()
    } catch {
      case pe: ParseError => throw TemplateParseException(pe.position.index, parser.formatError(pe))
    }
  }

  def parse(fullContents: String)(implicit lineSeparator: LineSeparator): MustacheBlock = {
    val allAsts = splitByDelim(fullContents).flatMap {
      case (s, delim) => parseWithDelim(handleStandalone(flattenMultilineTags(s, delim), delim), delim)
    }
    MustacheBlocks(convertToBlocks(allAsts): _*)
  }

  private def convertToBlocks(asts: Seq[MustacheAST])(implicit lineSeparator: LineSeparator): Seq[MustacheBlock] = {
    val openContextBlocks:  collection.mutable.Stack[ContextAST] = new collection.mutable.Stack()
    var contextContents: Seq[MustacheBlock] = Nil
    def appendOtherBlock(block: MustacheBlock): Option[MustacheBlock] = {
      if (openContextBlocks.isEmpty) {
        Some(block)
      } else {
        contextContents = contextContents :+ block
        None
      }
    }
    asts.foldLeft[Seq[MustacheBlock]](Seq.empty[MustacheBlock]) {
      case (acc: Seq[MustacheBlock], ast: MustacheAST) => ast match {
        case ast:Comment =>  acc
        case ast:ChangeDelimiter => acc
        case ast:EnterContext => {
          openContextBlocks.push(ast)
          acc
        }
        case ast:EnterNotContext => {
          openContextBlocks.push(ast)
          acc
        }
        case ExitContext(exitStartPos, contextName, _) => {
          if (openContextBlocks.isEmpty) {
            throw new TemplateParseException(exitStartPos, "Unexpected end of context")
          }
          val contextBlock = openContextBlocks.pop() match {
            case EnterContext(pos, `contextName`, _) => InContext(pos, contextName, MustacheBlocks(contextContents : _*))
            case EnterNotContext(pos, `contextName`, _) => InInverseContext(pos, contextName, MustacheBlocks(contextContents: _*))
            case _ => throw new TemplateParseException(exitStartPos, "Unexpected end of context")
          }
          contextContents = Nil
          acc :+ contextBlock
        }
        case Variable(pos, varName, _) => acc ++ appendOtherBlock(MustacheBlock.Variable(pos, varName))
        case VariableNoEscape(pos, varName, _) => acc ++ appendOtherBlock(MustacheBlock.VariableNoEscape(pos, varName))
        case Partial(pos, partialName, _) => acc ++ appendOtherBlock(MustacheBlock.Partial(pos, partialName))
        case Contents(pos, value, _) => acc ++ appendOtherBlock(MustacheBlock.Contents(pos, value))
      }
    }
  }

  private[poustache] def flattenMultilineTags(s: String, delim: Delimiter)(implicit lineSeparator: LineSeparator) = {
    val commentContentsRegex: Regex = s"(?s)${Pattern.quote(delim.open)}\\s*!.*${Pattern.quote(delim.close)}".r
    commentContentsRegex.replaceAllIn(s, _.group(0).replaceAll(Pattern.quote(lineSeparator.value), " "))
  }


  private val validTag = CharPredicate.Visible -- '='
  private val validContents = CharPredicate.All -- EOI
  private val validContextName = CharPredicate.Visible -- "!>#/^<{}&=\\"
  private val validPartialName = CharPredicate.Visible -- "!>#^<{}&=\\"

}