# poustache
A mustache compiler + template manager for Scala, based on parboiled2

## Usage
``
val mustacheService = new MustacheRenderService(templatesRootDirectory, true)
mustacheService.template("pooh!").render("a" -> true, "b" -> "gruik")
``

## Not supported yet :
- dotted names
- lambdas

## Known bugs
- It does not really get standalone lines
- Nested objects in context are not looked up

[![Build Status](https://travis-ci.org/mauhiz/poustache.svg)](https://travis-ci.org/mauhiz/poustache)
