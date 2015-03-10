package net.mauhiz.poustache

import java.io.File

case class MustacheException(template: File, cause: Throwable) extends RuntimeException(s"Could not render template: $template", cause)
