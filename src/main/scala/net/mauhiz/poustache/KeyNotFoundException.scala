package net.mauhiz.poustache



case class KeyNotFoundException(pos: Int, key: String) extends RuntimeException(s"Key not found: $key at position:$pos")
