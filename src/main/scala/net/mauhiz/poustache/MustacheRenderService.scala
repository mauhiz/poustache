package net.mauhiz.poustache

import java.io.{File, FileNotFoundException}

import scala.collection.concurrent.TrieMap
import scala.io.{Codec, Source}
import scala.util.control.NonFatal

class MustacheRenderService(templateRootDirectory: File, precompile: Boolean)(implicit lineSeparator: LineSeparator = LineSeparator.System) {

  private val mustacheWorld: scala.collection.mutable.Map[File, (MustacheBlock, Long)] = TrieMap.empty


  if (precompile) {
    def getFileTree(f: File): Stream[File] = f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree) else Stream.empty)
    mustacheWorld ++= getFileTree(templateRootDirectory).filter(_.getName.endsWith(".mustache")).map(parseMustache)
  }

  private def parseMustache(f: File): (File, (MustacheBlock, Long)) = {
    if (!f.getAbsolutePath.startsWith(templateRootDirectory.getAbsolutePath)) {
      throw new IllegalArgumentException(s"Caught trying to creep up: $f")
    }
    try {
      val fileContents = Source.fromFile(f)(Codec.UTF8).mkString
      val mustacheRoot = MustacheParser.parse(fileContents)
      (f, (mustacheRoot, f.lastModified()))
    } catch {
      case NonFatal(e) => throw MustacheException(f, e)
    }
  }

  def mustacheFinder(f: File): MustacheBlock = {
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