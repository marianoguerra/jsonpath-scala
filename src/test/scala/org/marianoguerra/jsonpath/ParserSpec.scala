package org.marianoguerra.jsonpath

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.util.parsing.combinator.Parsers$Failure

class ParserSpec extends FlatSpec with ShouldMatchers {
  val parser = new Parser()


  "A Parser" should "parse a single field" in {
    def checkField(name: String) = {
      val result = parser.parse(parser.field, name).get
      result should be (Field(false, name))
    }

    checkField("age")
    checkField("Age")
    checkField("user_age")
    checkField("age9")
    checkField("Age9")
    checkField("user_age9")
    checkField("$age")
    checkField("$Age")
    checkField("$user_age")
    checkField("_age")
    checkField("_Age")
    checkField("_user_age")
  }

  it should "parse a single subscript field" in {
    def checkField(name: String) = {
      val result = parser.parse(parser.subscriptField, "['" + name + "']").get
      result should be (Field(false, name))
    }

    checkField("age")
    checkField("Age")
    checkField("user_age")
    checkField("age9")
    checkField("Age9")
    checkField("user_age9")
    checkField("$age")
    checkField("$Age")
    checkField("$user_age")
    checkField("_age")
    checkField("_Age")
    checkField("_user_age")
  }

  it should "parse a wildcard" in {
      val result = parser.parse(parser.anyField, "*").get
      result should be (AnyField())
  }

  it should "parse the root object" in {
      val result = parser.parse(parser.root, "$").get
      result should be (Root())
  }

  it should "parse array slice with just index" in {
    def check(index: Int) = {
      val result = parser.parse(parser.arraySlice, "[" + index + "]").get
      result should be (ArraySlice(index, index + 1))
    }

    check(1)
    check(42)
    check(12354)
  }

  it should "parse array access with just index" in {
    def check(name: String, index: Int) = {
      val result = parser.parse(parser.arrayAccess, name + "[" + index + "]").get
      info(result.toString)
      result should be (ArrayAccess(
        Field(false, name), ArraySlice(index, index + 1)))
    }

    check("foo", 1)
    check("BAR", 42)
    check("_baz1$2_asd", 12354)
  }

  it should "parse array access with subscript field and index" in {
    def check(name: String, index: Int) = {
    val code = "['" + name + "'][" + index + "]"
      val result = parser.parse(parser.arrayAccess, code).get
      info(code)
      info(result.toString)
      result should be (ArrayAccess(
        Field(false, name), ArraySlice(index, index + 1)))
    }

    check("foo", 1)
    check("BAR", 42)
    check("_baz1$2_asd", 12354)

    check("foo", -1)
    check("BAR", -42)
    check("_baz1$2_asd", -12354)
  }

  it should "parse dot field access" in {
    def check(name: String) = {
      val result = parser.parse(parser.dotField, "." + name).get
      info(result.toString)
      result should be (Field(false, name))
    }

    check("foo")
    check("BAR")
    check("_baz1$2_asd")
  }

  it should "parse simple root.child queries" in {
    def check(query: String, expected: Any) = {
      val result = parser.parse(parser.query, query).get
      info(query)
      info(result.toString)
      result should be (expected)
    }

    check("$.name", List(Field(false, "name")))
    check("$..name", List(Field(true, "name")))
    check("$['name']", List(Field(false, "name")))
    check("$.name[2]", List(ArrayAccess(Field(false, "name"), ArraySlice(2, 3))))
    check("$['name'][2]", List(ArrayAccess(Field(false, "name"), ArraySlice(2, 3))))
  }

  it should "parse expressions from http://goessner.net/articles/JsonPath/" in {
    def check(query: String, expected: Any) = {
      val result = parser.parse(parser.query, query).get
      info(query)
      info(result.toString)
      result should be (expected)
    }

    check("$.store.book[0].title", List(
      Field(false, "store"),
      ArrayAccess(Field(false, "book"), ArraySlice(0, 1)),
      Field(false, "title")
    ))

    check("$['store']['book'][0]['title']", List(
      Field(false, "store"),
      ArrayAccess(Field(false, "book"), ArraySlice(0, 1)),
      Field(false, "title")
    ))

    check("$..author", List(Field(true, "author")))
    check("$.store.*", List(Field(false, "store"), AnyField(false)))
    check("$.store..price", List(Field(false, "store"), Field(true, "price")))
    check("$..*", List(AnyField(true)))
    check("$.*", List(AnyField(false)))
    check("$..book[2]", List(ArrayAccess(Field(true, "book"), ArraySlice(2, 3))))

    check("$.book[*]", List(ArrayAccess(Field(false, "book"), AllArrayItems())))
    check("$..book[*]", List(ArrayAccess(Field(true, "book"), AllArrayItems())))

    check("$.store['store']..book['book'][0].title..title['title'].*..*.book[*]..book[*]", List(
      Field(false, "store"),
      Field(false, "store"),
      Field(true, "book"),
      ArrayAccess(Field(false, "book"), ArraySlice(0, 1)),
      Field(false, "title"),
      Field(true, "title"),
      Field(false, "title"),
      AnyField(false),
      AnyField(true),
      ArrayAccess(Field(false, "book"), AllArrayItems()),
      ArrayAccess(Field(true, "book"), AllArrayItems())
    ))
  }

  it should "fail elegantly" in {
    def check(query: String, expected: Any) = {
      val result = parser.parse(parser.query, query)
      info(query)
      result match {
        case failure: parser.Failure => {
          info("ok, expected failure: " + result.toString)
        }
        case other => {
          fail("expected Failure instance, got " + other.toString)
        }
      }
    }

    check("", None)
    check("asd", None)
    //check("$.[0]", None)
  }
}

// vim: set ts=4 sw=4 et:
