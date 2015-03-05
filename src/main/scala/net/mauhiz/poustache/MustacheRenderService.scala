package net.mauhiz.poustache

import java.io.{File, FileNotFoundException}

import net.mauhiz.poustache.MustacheModel._
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.collection.concurrent.TrieMap
import scala.io.{Codec, Source}
import scala.util.control.NonFatal

class MustacheRenderService(templateRootDirectory: File, precompile: Boolean) {

  private val mustacheWorld: scala.collection.mutable.Map[File, (MustacheRoot, Long)] = TrieMap.empty


  if (precompile) {
    def getFileTree(f: File): Stream[File] = f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree) else Stream.empty)
    mustacheWorld ++= getFileTree(templateRootDirectory).filter(_.getName.endsWith(".mustache")).map(parseMustache)
  }

  private def parseMustache(f: File): (File, (MustacheRoot, Long)) = {
    if (!f.getAbsolutePath.startsWith(templateRootDirectory.getAbsolutePath)) {
      throw new IllegalArgumentException(s"Caught trying to creep up: $f")
    }
    try {
      val fileContents = Source.fromFile(f)(Codec.UTF8).mkString
      val parser = new MustacheParser(fileContents)
      try {
        import org.parboiled2.Parser.DeliveryScheme.Throw
        val mustacheRoot = parser.mustache.run()
        (f, (mustacheRoot, f.lastModified()))
      } catch {
        case pe: ParseError => {
          Console.err.println(parser.formatError(pe))
          throw MustacheException(f, pe)
        }
      }
    } catch {
      case NonFatal(e) => throw MustacheException(f, e)
    }
  }

  def mustacheFinder(f: File): MustacheRoot = {
    if (!f.exists() || !f.canRead) {
      mustacheWorld -= f
      throw new FileNotFoundException(f.getPath)
    }

    val (mustacheRoot, lastUpdated) = mustacheWorld.getOrElseUpdate(f, parseMustache(f)._2)
    if (lastUpdated < f.lastModified()) {
      val recentlyParsed = parseMustache(f)
      mustacheWorld += recentlyParsed
      recentlyParsed._2._1
    } else {
      mustacheRoot
    }
  }

  def template(template: String) = new MustacheTemplate(new File(templateRootDirectory, template).getCanonicalFile)

  class MustacheTemplate(templatePath: File) {
    def render(args: Iterable[(String, Any)]): String = {
      val mustacheRoot = mustacheFinder(templatePath)
      val mustacheContext = new MustacheContext(args.toMap)
      try {
        mustacheRoot.render(mustacheFinder, templatePath, mustacheContext)
      } catch {
        case NonFatal(e) => throw MustacheException(templatePath, e)
      }
    }
  }

}