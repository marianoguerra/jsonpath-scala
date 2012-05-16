package org.marianoguerra.jsonpath

abstract class JsonPath[T] {
  def query(query: String, obj: String): Either[Error, T]
  def query(query: String, obj: T): Either[Error, T]
}

// vim: set ts=4 sw=4 et:
