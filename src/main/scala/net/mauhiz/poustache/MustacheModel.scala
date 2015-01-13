package net.mauhiz.poustache

import java.io.File

import scala.util.control.NonFatal

object MustacheModel {

  case class MustacheException(template: File, cause: Throwable) extends RuntimeException(s"Could not render template: $template", cause)

  case class KeyNotFoundException(pos: Int, key: String) extends RuntimeException(s"Key not found: $key at position:$pos")

  sealed trait MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext): String
  }

  case class MustacheRoot(subBlocks: MustacheBlock*) extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = subBlocks.map(_.render(mustacheWorld, currentPath, context)).mkString
  }

  case class ContentsBlock(contents: String) extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = contents
  }

  case object CommentBlock extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = ""
  }

  case class ContextBlock(pos: Int, contextName: String, subBlocks: MustacheBlock*) extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = context.context(contextName, subBlocks.map(_.render(mustacheWorld, currentPath, context)).mkString)
  }

  case class InverseContextBlock(pos: Int, contextName: String, subBlocks: MustacheBlock*) extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = context.notContext(contextName, subBlocks.map(_.render(mustacheWorld, currentPath, context)).mkString)
  }

  case class PartialBlock(pos: Int, partialName: String) extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = {
      val partialPath = new File(currentPath.getParentFile.getAbsolutePath, s"$partialName.mustache").getCanonicalFile
      try {
        mustacheWorld(partialPath).render(mustacheWorld, partialPath, context)
      } catch {
        case NonFatal(e) => {
          throw MustacheException(partialPath, e)
        }
      }
    }
  }

  case class VariableBlock(pos: Int, key: String) extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = {
      context.printEscape(pos, key)
    }
  }

  case class VariableNoEscapeBlock(pos: Int, key: String) extends MustacheBlock {
    def render(mustacheWorld: File => MustacheRoot, currentPath: File, context: MustacheContext) = context.printNoEscape(pos, key)
  }
}
