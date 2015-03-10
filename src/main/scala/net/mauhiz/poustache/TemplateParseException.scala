package net.mauhiz.poustache

case class TemplateParseException(pos: Int, message: String, cause: Throwable = null) extends Exception(s"$message at index $pos", cause)