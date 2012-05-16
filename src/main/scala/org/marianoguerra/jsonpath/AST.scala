package org.marianoguerra.jsonpath

class BaseQuery

case class Root() extends BaseQuery

case class AnyField(val recursive: Boolean = false) extends BaseQuery

case class Field(val recursive: Boolean, val name: String) 
              extends BaseQuery

case class Fields(val recursive: Boolean, val names: String*) 
              extends BaseQuery

case class ArrayAccess(val field: Field, val slice: BaseArraySlice)
              extends BaseQuery

class BaseArraySlice() extends BaseQuery
case class AllArrayItems() extends BaseArraySlice

case class ArraySlice(
  val start: Int,
  val end: Int,
  val step: Int = 1)
            extends BaseArraySlice

// vim: set ts=4 sw=4 et:
