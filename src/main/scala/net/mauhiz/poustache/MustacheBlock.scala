package net.mauhiz.poustache

import java.io.File

import scala.util.control.NonFatal

sealed trait MustacheBlock {

  def render(mustacheWorld: File => MustacheBlock, currentPath: File, context: MustacheContext): String
}

object MustacheBlock {
  
  case class MustacheBlocks(blocks: MustacheBlock*) extends MustacheBlock {
    def render(mustacheWorld: (File) => MustacheBlock, currentPath: File, context: MustacheContext): String = {
      blocks.map(_.render(mustacheWorld, currentPath, context)).mkString
    }
  }

  case class InContext(pos: Int, contextName: String, inner: MustacheBlock) extends MustacheBlock {
    def render(mustacheWorld: (File) => MustacheBlock, currentPath: File, context: MustacheContext): String = {
      context.context(contextName, inner.render(mustacheWorld, currentPath, context))
    }
  }

  case class InInverseContext(pos: Int, contextName: String, inner: MustacheBlock) extends MustacheBlock {
    def render(mustacheWorld: (File) => MustacheBlock, currentPath: File, context: MustacheContext): String = {
      context.notContext(contextName, inner.render(mustacheWorld, currentPath, context))
    }
  }

  case class Contents(pos: Int, value: String) extends MustacheBlock {
    def render(mustacheWorld: (File) => MustacheBlock, currentPath: File, context: MustacheContext): String = value
  }

  case class Partial(pos: Int, partialName: String) extends MustacheBlock {

    def render(mustacheWorld: (File) => MustacheBlock, currentPath: File, context: MustacheContext): String = {
      val parentPath = currentPath.getParentFile.getAbsolutePath
      val partialPath = new File(parentPath, s"$partialName.mustache").getCanonicalFile
      try {
        mustacheWorld(partialPath).render(mustacheWorld, partialPath, context)
      } catch {
        case NonFatal(e) => {
          throw MustacheException(partialPath, e)
        }
      }
    }
  }
  
  case class Variable(pos: Int, varName: String) extends MustacheBlock {
    def render(mustacheWorld: (File) => MustacheBlock, currentPath: File, context: MustacheContext): String = {
      context.printEscape(pos, varName)
    }
  }
  
  case class VariableNoEscape(pos: Int, varName: String) extends MustacheBlock {
    def render(mustacheWorld: (File) => MustacheBlock, currentPath: File, context: MustacheContext): String = {
      context.printNoEscape(pos, varName)
    }
  }

}
