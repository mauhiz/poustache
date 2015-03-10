package net.mauhiz.poustache

import scala.util.Properties

case class LineSeparator(value: String)

object LineSeparator {
  val Unix = LineSeparator("\n")
  val System = LineSeparator(Properties.lineSeparator)
}
