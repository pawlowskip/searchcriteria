package com.pp.searchcriteria.serialization
import com.pp.searchcriteria.core.SearchCriteria
import com.pp.searchcriteria.core.SearchCriteria.{Equal, Or}
import com.pp.searchcriteria.querystring.QueryString.QSParam
import com.pp.searchcriteria.serialization.SearchCriteriaDeserializationUtils._
import com.pp.searchcriteria.serialization.Serialization.fromUpickleReader
import com.pp.searchcriteria.serialization.Deserializer._
import com.pp.searchcriteria.serialization.Deserializer.DeserializerBuilder._
import upickle.default._
import utest.TestSuite
import utest._

/**
  * Created by pp on 8/10/16.
  */
object SearchCriteriaDeserializationUtilsTest extends TestSuite {

  val tests = this {
    "Test [1] - SearchCriteriaDeserializationUtils checkAndTransformDeserializer" - {
      val des = checkAndTransformDeserializer(
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

    "Test [2] - SearchCriteriaDeserializationUtils multiValueDeserializer" - {

      val des1 = single(("1", "1"), Equal(1))
      val des2 = single(("2", "2"), Equal(2))
      val des3 = single(("3", "3"), Equal(3))
      val des4 = oneOfToken(Seq(("4", "4") -> Equal(4), ("5", "5") -> Equal(5)))


      val des: Deserializer[QSParam, SearchCriteria[Int]] = multiValueDeserializer(
        _._1 == "Ala",
        _._2.toInt,
        "Key should be 'Ala'",
        Seq(des1, des2, des3, des4),
        Or(_)
      )

      des.deserialize(Seq(("Ala", "3"), ("1", "1"), ("2", "2"), ("3", "3"))) ==>
        Ok(Or(Seq(Equal(1), Equal(2), Equal(3))), emptyInput, 4, Nil)

      des.deserialize(Seq(("Ala", "3"), ("1", "1"), ("4", "4"), ("3", "3"))) ==>
        Ok(Or(Seq(Equal(1), Equal(4), Equal(3))), emptyInput, 4, Nil)

      des.deserialize(Seq(("Ala", "4"), ("1", "1"), ("2", "2"), ("7", "7"), ("3", "3"))) ==>
        Fail(
          "Fail during deserializeTimes. Processed 2 times, but should be 4.",
          Seq("7" -> "7", "3" -> "3"),
          List(
            "Could not match any of deserializers in oneOf.",
            "Input token: [(7,7)] should be [(1,1)].",
            "Input token: [(7,7)] should be [(2,2)].",
            "Input token: [(7,7)] should be [(3,3)].",
            "Could not match any of tokens in oneOfTokens.",
            "Input token: [(7,7)] should be [(4,4)].",
            "Input token: [(7,7)] should be [(5,5)].")
        )

    }
  }
}
