package net.mauhiz.poustache

import net.mauhiz.poustache.MustacheModel.KeyNotFoundException

import scala.util.Try

case class MustacheContext(initialContext: Any, strict: Boolean = true) {

  import MustacheContext._

  var contextStack: Seq[Any] = Vector(initialContext)

  def context(contextName: String, innerContent: => String): String = {
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

  def notContext(contextName: String, innerContent: => String): String = {
    lookup(contextName) match {
      case None | Some(false) => innerContent
      case Some(trav: TraversableOnce[_]) if trav.isEmpty => innerContent
      case Some(opt: Option[_]) if opt.isEmpty => innerContent
      case _ => ""
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

  private def lookup(key: String): Option[Any] = {
    val keyParts = key.split('.')
    keyParts.headOption match {
      case None => contextStack.headOption
      case Some(headKey) =>
        for (item <- contextStack) {
          val lookedUp = singleLookup(headKey, item)
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
