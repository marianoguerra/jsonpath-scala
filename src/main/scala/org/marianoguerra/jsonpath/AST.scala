package org.marianoguerra.jsonpath

class BaseQuery
class BaseField extends BaseQuery

case class Root() extends BaseField

case class AnyField(val recursive: Boolean = false) extends BaseQuery

case class Field(val recursive: Boolean, val name: String) 
              extends BaseField

case class ArrayAccess(val field: BaseField, val slice: BaseArraySlice)
              extends BaseQuery

class BaseArraySlice() extends BaseQuery
case class AllArrayItems() extends BaseArraySlice

case class ArraySlice(
  val start: Int,
  val end: Int,
  val step: Int = 1)
            extends BaseArraySlice

// vim: set ts=4 sw=4 et:
