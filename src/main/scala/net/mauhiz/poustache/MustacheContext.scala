package net.mauhiz.poustache

import scala.util.Try

case class MustacheContext(initialContext: Any, strict: Boolean = true) {

  import net.mauhiz.poustache.MustacheContext._

  var contextStack: Seq[Any] = Vector(initialContext)

  def context(contextName: String, innerContent: => String): String = {
    contextName.indexOf('.') match {
      case dotIndex if dotIndex >= 0 && contextName.size > 1 => {
        val headContextName = contextName.substring(0, dotIndex)
        val tailContextName = contextName.substring(dotIndex + 1)
        context(headContextName, context(tailContextName, innerContent))
      }
      case -1 =>
        lookup(contextName) match {
          case None | Some(false) => ""
          case Some(true)         => innerContent
          case Some(trav: TraversableOnce[_]) => trav.map {
            seqItem =>
              contextStack = seqItem +: contextStack
              val out = innerContent
              contextStack = contextStack.tail
              out
          }.mkString("")
          case Some(mb: Option[_]) => mb.map {
            item =>
              contextStack = item +: contextStack
              val out = innerContent
              contextStack = contextStack.tail
              out
          }.mkString("")
          case Some(item: Any) => {
            contextStack = item +: contextStack
            val out = innerContent
            contextStack = contextStack.tail
            out
          }
        }
    }
  }

  def notContext(contextName: String, innerContent: => String): String = {
    contextName.indexOf('.') match {
      case dotIndex if dotIndex >= 0 && contextName.size > 1 => {
        val headContextName = contextName.substring(0, dotIndex)
        val tailContextName = contextName.substring(dotIndex + 1)
        // FIXME the logic is wrong here.
        notContext(headContextName, notContext(tailContextName, innerContent))
      }
      case -1 =>
        lookup(contextName) match {
          case None | Some(false) => innerContent
          case Some(trav: TraversableOnce[_]) if trav.isEmpty => innerContent
          case Some(opt: Option[_]) if opt.isEmpty => innerContent
          case _ => ""
        }
    }
  }

  def printEscape(pos: Int, key: String): String = escape(printNoEscape(pos, key))

  def printNoEscape(pos: Int, key: String): String = {
    lookup(key) match {
      case None => if (strict) throw KeyNotFoundException(pos, key) else ""
      case Some(Some(item)) => item.toString
      case Some(item)       => item.toString
    }
  }

  private def lookup(key: String, withContext: Seq[Any] = contextStack): Option[Any] = {
    if (key == ".") {
      contextStack.headOption
    } else if (key.isEmpty || key.endsWith(".")) {
      throw new KeyNotFoundException(-1, "")
    } else {
      for (item <- contextStack) {
        val lookedUp = singleLookup(key, item)
        if (lookedUp.isDefined) return lookedUp
      }
      if (strict) throw KeyNotFoundException(-1, key) else None
    }
  }

  private def singleLookup(key: String, context: Any): Option[Any] = {
    context match {
      case map: Map[String, _]      => map.get(key)
      case trav: TraversableOnce[_] => Some(trav)
      case opt: Option[_]           => Some(opt)
      case boo: Boolean             => Some(boo)
      case any: AnyRef => {
        Try {
          any.getClass.getMethod(key).invoke(any)
        }.toOption
      }
      case _ => None
    }
  }
}

object MustacheContext {

  private def escape(v: String): String = v.foldLeft(StringBuilder.newBuilder)((acc, ch) => sanitize(ch, acc)).toString()

  private def sanitize(ch: Char, buffer: StringBuilder): StringBuilder = {
    escapeSingleChar(ch).fold(buffer.append(ch))(buffer.append)
  }
  private[poustache] val escapeSingleChar: (Char) => Option[String] = ({
    case '\'' => "&#39;"
    case '"'  => "&quot;"
    case '&'  => "&amp;"
    case '<'  => "&lt;"
    case '>'  => "&gt;"
  }: PartialFunction[Char, String]).lift
}
