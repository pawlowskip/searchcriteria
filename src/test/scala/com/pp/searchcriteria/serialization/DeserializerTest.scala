package com.pp.searchcriteria.serialization
import com.pp.searchcriteria.serialization.Deserializer._
import com.pp.searchcriteria.serialization.Deserializer.DeserializerOps._
import utest.TestSuite
import utest._

/**
  * Created by pp on 5/24/16.
  */
object DeserializerTest extends TestSuite {

  val tests = this {
    "Test [1] - Deserializer single" - {
      val token = "token[a]"
      val d = single('a', token)
      val res = d.deserialize("a")
      assert(res == Ok(token, "", 1, Nil))
    }

    "Test [2] - Deserializer single with transformer" - {
      val d = single('9')(_.toString.toInt)
      val res = d.deserialize("9")
      assert(res == Ok(9, emptyInput, 1, Nil))
    }

    "Test [3] - Deserializer check" - {
      val d = check((c: Char) => c.isDigit, failMessage = "Should be digit.")(identity)

      * - {assert(d.deserialize("1") == Ok('1', emptyInput, 1, Nil))}
      * - {assert(d.deserialize("d").isInstanceOf[Fail[Char]])}
    }

    "Test [4] - Deserializer isExhausted" - {
      val d = isExhausted[Char]

      * - {assert(d.deserialize(emptyInput[Char]) == Ok(true, emptyInput[Char], 0, Nil))}
      * - {assert(d.deserialize("dasd").isInstanceOf[Fail[Char]])}
    }

    "Test [5a] - Deserializer transformWhile" - {
      val predicate = (c: Char) => c.isDigit
      val d = transformWhile[Char, Char](predicate, identity)

      val res1 = d.deserialize("123d")
      assert(
        res1 ==
          Ok(
            Seq('1', '2', '3'),
            "d",
            3,
            List("Token: [d] not satisfy predicate: Token not satisfy predicate in foldWhile. Back to previous result.")
          )
      )
    }

    "Test [5b] - Deserializer transformWhile" - {
      val predicate = (c: Char) => c.isDigit
      val d = transformWhile[Char, Char](predicate, identity)

      val res2 = d.deserialize(emptyInput[Char])
      assert(
        res2 ==
          Ok(
            Seq[Char](),
            emptyInput[Char],
            0,
            List("The input is empty. There is no token to parse.")
          )
      )
    }

    "Test [5c] - Deserializer transformWhile" - {
      val predicate = (c: Char) => c.isDigit
      val d = transformWhile[Char, Char](predicate, identity)

      val res3 = d.deserialize("123")
      assert(
        res3 ==
          Ok(
            List('1', '2', '3'),
            emptyInput[Char],
            3,
            List("The input is empty. There is no token to parse.")
          )
      )
    }

    "Test [6] - Deserializer zeroOrMore" - {
      val d = zeroOrMore[Char, Int]('!', seq => seq.size)

      val res1 = d.deserialize("no")
      val res2 = d.deserialize(emptyInput[Char])
      val res3 = d.deserialize("!!!!!!")
      val res4 = d.deserialize("d!!!!!!")
      val res5 = d.deserialize("!!!!!!sss!!!!")

      res1 ==> Ok(0, "no", 0, List("Token: [n] not satisfy predicate: Token not satisfy predicate in foldWhile. Back to previous result."))
      res2 ==> Ok(0, emptyInput[Char], 0, List("The input is empty. There is no token to parse."))
      res3 ==> Ok(6, emptyInput[Char], 6, List("The input is empty. There is no token to parse."))
      res4 ==> Ok(0, "d!!!!!!", 0, List("Token: [d] not satisfy predicate: Token not satisfy predicate in foldWhile. Back to previous result."))
      res5 ==> Ok(6, "sss!!!!", 6, List("Token: [s] not satisfy predicate: Token not satisfy predicate in foldWhile. Back to previous result."))
    }

    "Test [7] - Deserializer oneOrMore" - {
      val d = oneOrMore[Char, Int]('!', seq => seq.size)

      d.deserialize("!d") ==>
        Ok(1, "d", 1, List("Token: [d] not satisfy predicate: Token not satisfy predicate in foldWhile. Back to previous result."))

      d.deserialize("no") ==>
        Fail(
          "Could not match at least one token: [!].",
          "no",
          List("Token: [n] not satisfy predicate: Token not satisfy predicate in foldWhile. Back to previous result.")
        )

      d.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]] ==> true

      d.deserialize("!!!!!!") ==> Ok(6, emptyInput[Char], 6, List("The input is empty. There is no token to parse."))

      d.deserialize("d!!!!!!").isInstanceOf[Fail[Char]] ==> true

      d.deserialize("!!!!!!sss!!!!") ==>
        Ok(6, "sss!!!!", 6, List("Token: [s] not satisfy predicate: Token not satisfy predicate in foldWhile. Back to previous result."))
    }

//    "Test [8] - Deserializer times" - {
//      val d = times[Char, Char]('!', 2, '!')
//
//      d.deserialize("!!") ==> Ok('!', emptyInput[Char], 2, List())
//      d.deserialize("!!!").isInstanceOf[Fail[Char]] ==> true
//      d.deserialize("a!!!").isInstanceOf[Fail[Char]] ==> true
//      d.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]] ==> true
//      d.deserialize("!!") ==> Ok('!', emptyInput[Char], 2, Nil)
//    }

    "Test [9] - Deserializer oneOf" - {
      val seq = Seq(('1', '1'), ('2', '2'))
      val d = oneOfToken(seq)

      * - {assert(d.deserialize("1") == Ok('1', emptyInput[Char], 1, Nil))}
      * - {assert(d.deserialize("2") == Ok('2', emptyInput[Char], 1, Nil))}
      * - {assert(d.deserialize("12") == Ok('1', "2", 1, Nil))}
      * - {assert(d.deserialize("3").isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]])}
    }

    "Test [10] - Deserializer andThen" - {
      val d1 = single[Char, Char]('1', '1')
      val d2 = single[Char, Char]('2', '2')
      val d3 = d1.andThen(d2)(_.toString + _.toString)

      * - {assert(d3.deserialize("12") == Ok("12", emptyInput[Char], 2, Nil))}
      * - {assert(d3.deserialize("12d") == Ok("12", "d", 2, Nil))}
      * - {assert(d3.deserialize("a12").isInstanceOf[Fail[Char]])}
      * - {assert(d3.deserialize("1a2").isInstanceOf[Fail[Char]])}
      * - {assert(d3.deserialize("112").isInstanceOf[Fail[Char]])}
      * - {assert(d3.deserialize("21").isInstanceOf[Fail[Char]])}
      * - {assert(d3.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]])}
    }

    "Test [11] - Deserializer end" - {
      val d = single('a', 'a').end

      * - {assert(d.deserialize("a") == Ok('a', emptyInput[Char], 1, Nil))}
      * - {assert(d.deserialize("aa").isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]])}
    }

    "Test [10] - Deserializer foldBackward" - {
      val d1 = single[Char, Char => Int]('1', (c: Char) => c.toString.toInt)
      val d2 = single[Char, Char]('2', '2')
      val d3 = foldBackward(d1, d2)

      * - {assert(d3.deserialize("12") == Ok(2, emptyInput[Char], 2, Nil))}
      * - {assert(d3.deserialize("12d") == Ok(2, "d", 2, Nil))}
      * - {assert(d3.deserialize("a12").isInstanceOf[Fail[Char]])}
      * - {assert(d3.deserialize("1a2").isInstanceOf[Fail[Char]])}
      * - {assert(d3.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]])}
    }

    "Test [11] - Deserializer deserializeTimes" - {
      val one = single('1', '1')
      val fiveOnesDes = deserializeTimes(one, 5)(o => true)
      fiveOnesDes.deserialize("11111") ==> Ok("11111".toList, emptyInput, 5, Nil)
      fiveOnesDes.deserialize("111111") ==> Ok("11111".toList, "1", 5, Nil)
      fiveOnesDes.deserialize("1111") ==>
        Fail(
          "Fail during deserializeTimes. Processed 4 times, but should be 5.",
          emptyInput,
          List("The input is empty. There is no token to parse.")
        )

      fiveOnesDes.deserialize("1111d") ==>
        Fail(
          "Fail during deserializeTimes. Processed 4 times, but should be 5.",
          "d",
          List("Input token: [d] should be [1].")
        )
    }

  }
}