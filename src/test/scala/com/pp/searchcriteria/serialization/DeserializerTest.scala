package com.pp.searchcriteria.serialization
import com.pp.searchcriteria.serialization.Deserializer._
import com.pp.searchcriteria.serialization.Deserializer.DeserializerBuilder._
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

    "Test [5] - Deserializer foldWhile" - {
      val predicate = (c: Char) => c.isDigit
      val d = foldWhile[Char, Char](predicate, identity)

      * - {assert(d.deserialize("123d") == Ok(Seq('1', '2', '3'), "d", 3, Nil))}
      * - {assert(d.deserialize(emptyInput[Char]) == Ok(Seq[Char](), emptyInput[Char], 0, Nil))}
      * - {assert(d.deserialize("123") == Ok(Seq('1', '2', '3'), emptyInput[Char], 3, Nil))}
    }

    "Test [6] - Deserializer zeroOrMore" - {
      val d = zeroOrMore[Char, Int]('!', seq => seq.size)

      * - {assert(d.deserialize("no") == Ok(0, "no", 0, Nil))}
      * - {assert(d.deserialize(emptyInput[Char]) == Ok(0, emptyInput[Char], 0, Nil))}
      * - {assert(d.deserialize("!!!!!!") == Ok(6, emptyInput[Char], 6, Nil))}
      * - {assert(d.deserialize("d!!!!!!") == Ok(0, "d!!!!!!", 0, Nil))}
      * - {assert(d.deserialize("!!!!!!sss!!!!") == Ok(6, "sss!!!!", 6, Nil))}
    }

    "Test [7] - Deserializer oneOrMore" - {
      val d = oneOrMore[Char, Int]('!', seq => seq.size)

      * - {assert(d.deserialize("!d") == Ok(1, "d", 1, Nil))}
      * - {assert(d.deserialize("no").isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize("!!!!!!") == Ok(6, emptyInput[Char], 6, Nil))}
      * - {assert(d.deserialize("d!!!!!!").isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize("!!!!!!sss!!!!") == Ok(6, "sss!!!!", 6, Nil))}
    }

    "Test [8] - Deserializer times" - {
      val d = times[Char, Char]('!', 2, '!')

      * - {assert(d.deserialize("!!") == Ok('!', emptyInput[Char], 2, Nil))}
      * - {assert(d.deserialize("!!!").isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize("a!!!").isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize(emptyInput[Char]).isInstanceOf[Fail[Char]])}
      * - {assert(d.deserialize("!!") == Ok('!', emptyInput[Char], 2, Nil))}
    }

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

  }
}