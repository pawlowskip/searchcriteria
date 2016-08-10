package com.pp.searchcriteria.serialization
import com.pp.searchcriteria.serialization.SearchCriteriaDeserializationUtils._
import com.pp.searchcriteria.serialization.Serialization.fromUpickleReader
import com.pp.searchcriteria.serialization.Deserializer._
import upickle.default._
import utest.TestSuite
import utest._

/**
  * Created by pp on 8/10/16.
  */
object SearchCriteriaDeserializationUtilsTest extends TestSuite {

  val tests = this {
    "Test [1] - SearchCriteriaDeserializationUtils singleValueDeserializer" - {
      val des = singleValueDeserializer(
        predicate = _._1 == "Ala",
        failMessage = "Key should be 'Ala'",
        reader = fromUpickleReader[Int],
        transformer = (i: Int) => i.toString
      )

      des.deserialize(Seq(("Ala", "3"))) ==> Ok("3", emptyInput, 1, Nil)

      des.deserialize(Seq(("Al", "3"))) ==>
        Fail("Token: [(Al,3)] not satisfy predicate: Key should be 'Ala'", Seq(("Al", "3")), Nil)

      des.deserialize(Seq(("Ala", "dupa"))) ==>
        Fail(
          "Error during transformation of input token: [(Ala,dupa)] in deserializer. Error: jawn.ParseException: " +
            "expected json value got d (line 1, column 1) (input: dupa)",
          Seq(("Ala", "dupa")),
          Nil)
      
    }
  }
}
