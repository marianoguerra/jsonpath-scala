package org.marianoguerra.jsonpath

import scala.util.parsing.combinator._

class Parser extends JavaTokenParsers {

    def query: Parser[List[BaseQuery]] =
      root ~ rep(childAccess) ^^ 
        { case root ~ childAccess => childAccess }
    
    def childAccess: Parser[BaseQuery] = (
      subArrayAccess
      | dotField
      | deepField
      | dotAny
      | deepAny
      | subscriptField)

    def anyField: Parser[AnyField] = "*" ^^ ((x) => AnyField())

    def root: Parser[Root] = "$" ^^ ((x) => Root())

    // field parsers

    def field: Parser[Field] = 
      "[$a-zA-Z_][0-9a-zA-Z_$]*".r ^^ ((name) => Field(false, name))

    def subscriptField: Parser[Field] = 
      "['" ~> field <~ "']"

    // child accessors

    def dotField: Parser[Field] =
      "." ~> field

    def deepField: Parser[Field] =
      ".." ~> field ^^ ((field) => Field(true, field.name))

    def dotAny: Parser[AnyField] =
      ".*" ^^ ((_) => AnyField(false))

    def deepAny: Parser[AnyField] =
      "..*" ^^ ((_) => AnyField(true))

    // array parsers

    def arraySlice: Parser[BaseArraySlice] =
      arraySlice1 | allArraySlice

    def arraySlice1: Parser[ArraySlice] =
      "[" ~> integer <~ "]" ^^ 
        ((start) => ArraySlice(start, start + 1, 1))

    def allArraySlice: Parser[AllArrayItems] =
      "[*]" ^^ ((_) => AllArrayItems())

    def arrayFieldAccess: Parser[ArrayAccess] =
      field ~ arraySlice ^^ 
        { case field ~ slice => ArrayAccess(field, slice) }

    def deepArrayFieldAccess: Parser[ArrayAccess] =
      ".." ~> (field ~ arraySlice) ^^ 
        { case Field(_, name) ~ slice => ArrayAccess(Field(true, name), slice) }

    def arraySubscriptAccess: Parser[ArrayAccess] =
      subscriptField ~ arraySlice ^^ 
        { case field ~ slice => ArrayAccess(field, slice) }

    def arrayAccess: Parser[ArrayAccess] = 
      arrayFieldAccess | arraySubscriptAccess

    // .name[index] or ['name'][index] for subacces, for example
    // $.name[1] or $['name'][0]
    def subArrayAccess: Parser[ArrayAccess] = 
      ("." ~> arrayFieldAccess) | arraySubscriptAccess | deepArrayFieldAccess

    // generic parsers
    def integer: Parser[Int] =
      "-?[0-9]+".r ^^ ((num) => num.toInt)

    def compile(jsonpath: String): ParseResult[List[BaseQuery]] = {
      parse(query, jsonpath)
    }

}

object Parser extends Parser


// vim: set ts=4 sw=4 et:
