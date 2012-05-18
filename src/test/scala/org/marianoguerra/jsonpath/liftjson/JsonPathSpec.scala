package org.marianoguerra.jsonpath.liftjson

import net.liftweb.json._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class JsonPathSpec extends FlatSpec with ShouldMatchers {
  val justOneItem = "{\"name\": \"mariano\"}"
  val justOneArray = "{\"name\": [1, 2, 3, 4, 5]}"
  val deepItem = "{\"person\": {\"name\": \"mariano\"}}"
  val objWithListOfObjs = "{\"persons\": [{\"name\": \"bob\"}, {\"name\": \"patrick\"}, {\"name\": \"sandy\"}]}"
  val listOfObjs = "[{\"name\": \"bob\"}, {\"name\": \"patrick\"}, {\"name\": \"sandy\"}]"

  it should "find a single field" in {
    JsonPath.query("$.name", justOneItem) should be (Right(JString("mariano")))
    JsonPath.query("$..name", justOneItem) should be (Right(JString("mariano")))

    JsonPath.query("$.age", justOneItem) should be (Right(JNothing))
    // XXX don't know if returning an empty object is the best thing
    JsonPath.query("$..age", justOneItem) should be (Right(JObject(List())))

    JsonPath.query("$.age.something", justOneItem) should be (Right(JNothing))
    JsonPath.query("$.name.something", justOneItem) should be (Right(JNothing))

    JsonPath.query("$['name']", justOneItem) should be (Right(JString("mariano")))
    JsonPath.query("$['age']", justOneItem) should be (Right(JNothing))

    JsonPath.query("$.age['something']", justOneItem) should be (Right(JNothing))
    JsonPath.query("$.name['something']", justOneItem) should be (Right(JNothing))

    JsonPath.query("$['age']['something']", justOneItem) should be (Right(JNothing))
    JsonPath.query("$['name']['something']", justOneItem) should be (Right(JNothing))
  }

  it should "find a deep attribute" in {
    JsonPath.query("$.person.name", deepItem) should be (Right(JString("mariano")))
    JsonPath.query("$['person'].name", deepItem) should be (Right(JString("mariano")))
    JsonPath.query("$['person']['name']", deepItem) should be (Right(JString("mariano")))
    JsonPath.query("$.person['name']", deepItem) should be (Right(JString("mariano")))
    JsonPath.query("$..name", deepItem) should be (Right(JString("mariano")))
  }

  it should "find and slice an array" in {
    JsonPath.query("$.name[2]", justOneArray) should be (Right(JArray(List(JInt(3)))))
    JsonPath.query("$.name[20]", justOneArray) should be (Right(JArray(List())))
    JsonPath.query("$.age[20]", justOneArray) should be (Right(JNothing))

    JsonPath.query("$['name'][2]", justOneArray) should be (Right(JArray(List(JInt(3)))))
    JsonPath.query("$['name'][20]", justOneArray) should be (Right(JArray(List())))
    JsonPath.query("$['age'][20]", justOneArray) should be (Right(JNothing))
  }

  it should "find and slice an array with a wildcard" in {
    JsonPath.query("$.name[*]", justOneArray) should be (Right(JArray(List(JInt(1), JInt(2), JInt(3), JInt(4), JInt(5)))))
  }

  it should "access elements inside arrays" in {
    val result = Right(JArray(List(JString("bob"), JString("patrick"), JString("sandy"))))
    JsonPath.query("$.persons[*].name", objWithListOfObjs) should be (result)
    // XXX should this return JNothing like jsonpath.js?
    JsonPath.query("$.persons.name", objWithListOfObjs) should be (result)
    // XXX this returns something really weird:
    // Right(JObject(List(JField(name,JString(bob)), JField(name,JString(patrick)), JField(name,JString(sandy)))))
    //JsonPath.query("$..name", objWithListOfObjs) should be (result)
    JsonPath.query("$.persons[1].name", objWithListOfObjs) should be (Right(JArray(List(JString("patrick")))))
    JsonPath.query("$.persons[10].name", objWithListOfObjs) should be (Right(JArray(List())))
    JsonPath.query("$.persons[1].foo", objWithListOfObjs) should be (Right(JArray(List())))
    // XXX shouldn't this return JNothing?
    JsonPath.query("$..foo", objWithListOfObjs) should be (Right(JObject(List())))
  }

  it should "allow array access on root" in {
    JsonPath.query("$[0]", listOfObjs) should be (Right(JArray(List(JObject(List(JField("name", JString("bob"))))))))
  }
}

// vim: set ts=4 sw=4 et:
