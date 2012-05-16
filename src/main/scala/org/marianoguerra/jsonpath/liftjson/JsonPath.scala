package org.marianoguerra.jsonpath.liftjson

import net.liftweb.json._

import org.marianoguerra.jsonpath
import org.marianoguerra.jsonpath.{ BaseQuery, Field, AllArrayItems, ArrayAccess, ArraySlice }

class JsonPath extends jsonpath.JsonPath[JValue] {
  implicit val formats = DefaultFormats // Brings in default date formats etc.

  val parser = jsonpath.Parser

  override def query(queryStr: String, obj: String): Either[jsonpath.Error, JValue] = {
    query(queryStr, parse(obj))
  }

  override def query(queryStr: String, obj: JValue): Either[jsonpath.Error, JValue] = {
    val compileResult = parser.compile(queryStr)

    if (compileResult.successful) {
      val compiledQuery = compileResult.get
      Right(filter(obj, compiledQuery))
    } else {
      Left(jsonpath.Error(compileResult.toString))
    }

  }

  private[liftjson] def filter(obj: JValue, query: List[BaseQuery]): JValue = {
    if (query.isEmpty) {
      obj
    } else {
      val nextFilter :: rest = query
      val newObj = filterOne(obj, nextFilter)
      filter(newObj, rest)
    }
  }

  private[liftjson] def filterOne(obj: JValue, query: BaseQuery): JValue = {
    val result = query match {
      case Field(false, name) => {
        val result = obj \ name

        if (obj.isInstanceOf[JArray] && !result.isInstanceOf[JArray]) {
          if (result != JNothing) {
            JArray(List(result))
          } else {
            JArray(List())
          }
        } else {
          result
        }
      }
      case Field(true, name) => {
        obj \\ name
      }
      case ArrayAccess(fieldQuery, slice) => {
        val fieldsMatched = filterOne(obj, fieldQuery)
        filterOne(fieldsMatched, slice)
      }
      case AllArrayItems() if (obj.isInstanceOf[JArray]) => obj
      case ArraySlice(start, end, step) if (obj.isInstanceOf[JArray]) => {
        val fromStartToEnd = obj.asInstanceOf[JArray].arr.slice(start, end)

        val slice = if (step != 1) {
          fromStartToEnd.grouped(step).toList.map(_.head)
        } else {
          fromStartToEnd
        }

        JArray(slice)
      }

      case other => JNothing
    }

    result match {
      case JField(_, value) => value
      case JArray(items) => {
        val newItems = items map {
          case JField(_, value) => value
          case other => other
        }

        JArray(newItems)
      }
      case other => other
    }
  }

}

object JsonPath extends JsonPath

// vim: set ts=4 sw=4 et:
