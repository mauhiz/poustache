package net.mauhiz.poustache

sealed trait ContextAST

sealed abstract class MustacheAST(startPos: Int, endPos: Int)

object MustacheAST {

  case class EnterContext(startPos: Int, contextName: String, endPos: Int) extends MustacheAST(startPos, endPos) with ContextAST

  case class EnterNotContext(startPos: Int, contextName: String, endPos: Int) extends MustacheAST(startPos, endPos) with ContextAST

  case class ExitContext(startPos: Int, contextName: String, endPos: Int) extends MustacheAST(startPos, endPos)

  case class Partial(startPos: Int, partialName: String, endPos: Int) extends MustacheAST(startPos, endPos)


  case class ChangeDelimiter(startPos: Int, newStartTag: String, newEndTag: String, endPos: Int) extends MustacheAST(startPos, endPos)

  case class Comment(startPos: Int, endPos: Int) extends MustacheAST(startPos, endPos)


  case class Contents(startPos: Int, value: String, endPos: Int) extends MustacheAST(startPos, endPos)

  case class Variable(startPos: Int, varName: String, endPos: Int) extends MustacheAST(startPos, endPos)

  case class VariableNoEscape(startPos: Int, varName: String, endPos: Int) extends MustacheAST(startPos, endPos)

}