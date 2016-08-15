package com.pp.searchcriteria.serialization

import scala.annotation.tailrec
import scala.util.control.NonFatal

/**
  * Created by pp on 8/7/16.
  */
trait Deserializer[Token, +O] {
  type Input = Deserializer.Input[Token]

  def deserialize(i: Input): Result[O, Token]

  def map[O2](g: O => O2): Deserializer[Token, O2] = Deserializer[Token, O2](input =>
    this.deserialize(input).map(g))

  def flatMap[O2](g: O => Deserializer[Token, O2]): Deserializer[Token, O2] =
    Deserializer[Token, O2](input =>
      this.deserialize(input) match {
        case Ok(result, inputLeft, tokensParsed, messageStack) =>
          g(result)
            .deserialize(inputLeft)
            .withTokensParsed(_ + tokensParsed)
            .withStackMessage(stack => messageStack ::: stack)
        case fail @ Fail(_, _, _) => fail
      }
    )

  def mapResult[A](g: Result[O, Token] => Result[A, Token]) = Deserializer[Token, A](input =>
    g(this.deserialize(input))
  )

  def flatMapResult[A](g: Result[O, Token] => Deserializer[Token, A]) = Deserializer[Token, A] { input =>
    val res = this.deserialize(input)
    g(res).deserialize(res.inputLeft)
  }

  def mapFail(f: Fail[Token] => Fail[Token]): Deserializer[Token, O] =
    mapResult{
      case fail @ Fail(_, _, _) => f(fail)
      case ok => ok
    }

}

object Deserializer {
  type Input[Token] = Iterable[Token]

  def emptyInput[Token] = Iterable.empty[Token]

  def input[Token](tokens: Token*): Input[Token] = Iterable(tokens: _*)

  object Messages {
    val emptyInput = "The input is empty. There is no token to parse."
  }


  implicit def toBuilder[Token, O](des: Deserializer[Token, O]): DeserializerBuilder[Token, O] =
    new DeserializerBuilder[Token, O] {
      override val deserializer = des
    }

  def apply[Token, O](f: Input[Token] => Result[O, Token]): Deserializer[Token, O] =
    new Deserializer[Token, O] {
      override def deserialize(i: Input): Result[O, Token] = f(i)
    }

  def always[Token, O](result: Result[O, Token]): Deserializer[Token, O] =
    Deserializer[Token, O](input => result)

  def failed[Token, O](cause: String): Deserializer[Token, O] = Deserializer[Token, O](input => Fail(cause, input, Nil))

  object DeserializerBuilder {

    def single[Token, O](token: Token)(transformer: Token => O): Deserializer[Token, O] =
      single(token, transformer(token))

    def single[Token, O](token: Token, out: O): Deserializer[Token, O] = Deserializer[Token, O] { input =>
      input.headOption match {
        case Some(token1) if token1 == token => Ok(out, input.tail, 1, Nil)
        case Some(token1) =>
          Fail(s"Input token: [$token1] should be [$token].", input, Nil)
        case None => Fail(Messages.emptyInput, input, Nil)
      }
    }

//    def singleWithNewToken[Token, NewToken, O](des: Deserializer[Token, O], f: NewToken => Token): Deserializer[NewToken, O] =
//      Deserializer[NewToken, O] { i =>
//        i.headOption match {
//          case Some(newToken) =>
//            val token: Token = f(newToken)
//            des.deserialize(input(token)) match {
//              case Ok(result, emptyInput, 1) => Ok(result, i.tail, 1)
//              case Fail(cause, inputLeft) => Fail(cause, i)
//            }
//          case None => Fail(Messages.emptyInput, i)
//        }
//      }


    def transform[Token, O](transformer: Token => O): Deserializer[Token, O] =
      Deserializer[Token, O] { input =>
        input.headOption match {
          case Some(token) =>
            try {
              val result = transformer(token)
              Ok(result, input.tail, 1, Nil)
            } catch {
              case NonFatal(e) =>
                Fail(s"Error during transformation of input token: [$token] in deserializer. Error: ${e.getMessage}",
                     input, Nil)
            }
          case None => Fail[Token](Messages.emptyInput, input, Nil)
        }
      }

    def check[Token, O](predicate: Token => Boolean, failMessage: String)
                       (transformer: Token => O): Deserializer[Token, O] =
      Deserializer[Token, O] { input =>
        input.headOption match {
          case Some(token) if predicate(token) =>
            try {
              Ok(transformer(token), input.tail, 1, Nil)
            } catch {
              case NonFatal(e) =>
                Fail(
                  s"Error during transformation of input token: [$token] in deserializer. Error: ${e.getMessage}",
                  input,
                  Nil
                )
            }
          case Some(token) => Fail(s"Token: [$token] not satisfy predicate: $failMessage", input, Nil)
          case None => Fail[Token](Messages.emptyInput, input, Nil)
        }
      }

//    def checkOutput[Token, O](predicate: O => Boolean, failMessage: String)
//                             (transformer: Token => O): Deserializer[Token, O] =
//      Deserializer[Token, O] { input =>
//        input.headOption match {
//          case Some(token) =>
//            try {
//              val result = transformer(token)
//              if (predicate(result)) Ok(result, input.tail, 1)
//              else Fail(s"Output: $result for token: $token not satisfy predicate.", input)
//            } catch {
//              case NonFatal(e) => Fail(s"Bad input for token. Error: ${e.getMessage}", input)
//            }
//          case None => Fail[Token](Messages.emptyInput, input)
//        }
//      }

    def isExhausted[Token]: Deserializer[Token, Boolean] =
      Deserializer[Token, Boolean](input =>
        if (input.isEmpty) Ok(true, input, 0, Nil)
        else Fail("Input not exhausted.", input, Nil)
      )

    def transformWhile[Token, O](predicate: Token => Boolean,
                                 transformer: Token => O): Deserializer[Token, Seq[O]] = {

      val check: Deserializer[Token, O] =
        DeserializerBuilder
          .check[Token, O](predicate, "Token not satisfy predicate in foldWhile. Back to previous result.")(transformer)

      @tailrec
      def transformWhile0(results: Seq[O], input: Deserializer.Input[Token], tokensParsed: Int): Result[Seq[O], Token] = {
        check.deserialize(input) match {
          case Ok(result, inputLeft, parsed, _) =>
            transformWhile0(results :+ result, inputLeft, tokensParsed + parsed)
          case Fail(cause, inputLeft, messageStack) =>
            Ok(results, inputLeft, tokensParsed, cause :: messageStack)
        }
      }
      Deserializer[Token, Seq[O]] { input =>
        transformWhile0(Seq(), input, 0)
      }
    }

    def zeroOrMore[Token, O](token: Token, transformer: Seq[Token] => O): Deserializer[Token, O] = {
      Deserializer[Token, O] { input =>
        val predicate = (t: Token) => t == token
        transformWhile[Token, Token](predicate, identity(_)).deserialize(input).map(transformer)
      }
    }

    def oneOrMore[Token, O](token: Token, transformer: Seq[Token] => O): Deserializer[Token, O] =
      zeroOrMore(token, transformer).mapResult {
        case result: Result[O, Token] if result.tokensParsed == 0 =>
          Fail(s"Could not match at least one token: [$token].", result.inputLeft, result.messageStack)
        case result: Result[O, Token] => result
      }

//    def times[Token, O](token: Token, n: Int, out: O): Deserializer[Token, O] =
//      zeroOrMore(token, (t: Seq[Token]) => out).mapResult {
//        case result: Result[O, Token] if result.tokensParsed != n =>
//          Fail(s"Token: [$token] matched ${result.tokensParsed} times. Should be: $n.", result.inputLeft, result.messageStack)
//        case result: Result[O, Token] => result
//      }
//
//    def times[Token, O](token: Token, n: Int)(transformer: Token => O): Deserializer[Token, Seq[O]] =
//      zeroOrMore(token, (t: Seq[Token]) => t.map(transformer)).mapResult {
//        case result: Result[O, Token] if result.tokensParsed != n =>
//          Fail(s"Token: [$token] matched ${result.tokensParsed} times. Should be: $n.", result.inputLeft, result.messageStack)
//        case result: Result[O, Token] => result
//      }

//    def processWhile[Token, O](des: Deserializer[Token, O])(predicate: O => Boolean): Deserializer[Token, Seq[O]] = {
//      def processWhile0(acc: Seq[O], parsed: Int): Deserializer[Token, Seq[O]] = {
//        des.flatMapResult {
//          case Ok(out, inputLeft, tokensParsed) if predicate(out) =>
//            processWhile0(acc :+ out, parsed + tokensParsed)
//          case Ok(out, inputLeft, tokensParsed) => Deserializer.always(Ok(acc, inputLeft, parsed))
//          case Fail(cause, inputLeft) => Deserializer.always(Ok(acc, inputLeft, parsed))
//        }
//      }
//      processWhile0(Seq.empty[O], 0)
//    }

    def deserializeTimes[Token, O](des: Deserializer[Token, O], n: Int)(predicate: O => Boolean): Deserializer[Token, Seq[O]] = {
      require(n > 0)
      def deserializeTimes0(acc: Seq[O], parsed: Int, times: Int): Deserializer[Token, Seq[O]] = {
        if (times < n) {
          des.flatMapResult {
            case Ok(out, inputLeft, tokensParsed, _) if predicate(out) =>
              deserializeTimes0(acc :+ out, parsed + tokensParsed, times + 1)
            case Ok(out, inputLeft, tokensParsed, _) =>
              deserializeTimes0(acc, parsed, times)
            case fail @ Fail(cause, inputLeft, messageStack) =>
              always(Fail(
                s"Fail during deserializeTimes. Processed $times times, but should be $n.",
                inputLeft,
                cause :: messageStack
              ))
          }
        } else Deserializer((input: Input[Token]) => Ok(acc, input, parsed, Nil))
      }
      deserializeTimes0(Seq.empty[O], 0, 0)
    }

    def oneOfToken[Token, O](tokens: Seq[(Token, O)]): Deserializer[Token, O] = tokens match {
      case Seq() => failed(s"Could not match any of tokens in oneOfTokens.")
      case (token, o) +: rest => single[Token, O](token, o).flatMapResult {
        case ok@Ok(_, _, _, _) => always(ok)
        case Fail(cause, _, messageStack) =>
          oneOfToken(rest).mapFail{ fail =>
            fail.withStackMessage(stack => cause :: messageStack ::: stack)
          }
      }
    }

    def oneOf[Token, O](deserializers: Seq[Deserializer[Token, O]]): Deserializer[Token, O] = deserializers match {
      case Seq() => failed(s"Could not match any of deserializers in oneOf.")
      case des +: rest => des.flatMapResult {
        case ok@Ok(_, _, _, _) => always(ok)
        case Fail(cause, _, messageStack) =>
          oneOf(rest).mapFail{ fail =>
            fail.withStackMessage(stack => cause :: messageStack ::: stack)
          }
      }
    }

    def foldBackward[Token, O1, O2](des1: Deserializer[Token, O2 => O1],
                                    des2: Deserializer[Token, O2]): Deserializer[Token, O1] = {
      for {
        res1 <- des1
        res2 <- des2
      } yield res1(res2)
    }

    def andThen[Token, O1, O2, O3](des1: Deserializer[Token, O1],
                                   des2: Deserializer[Token, O2])
                                  (resultBinder: (O1, O2) => O3): Deserializer[Token, O3] =
      for {
        res1 <- des1
        res2 <- des2
      } yield resultBinder(res1, res2)

    def or[Token, O](des1: Deserializer[Token, O], des2: Deserializer[Token, O]): Deserializer[Token, O] =
      des1.flatMapResult {
        case ok@Ok(_, _, _, _) => always(ok)
        case Fail(cause1, _, messageStack) => des2.mapResult {
          case ok: Ok[_, _] => ok
          case Fail(cause2, inputLeft, messageStack2) =>
            Fail(s"Could not match tokens: [$cause1], [$cause2]",
              inputLeft,
              cause1 :: messageStack ::: cause2 :: messageStack2)
        }
      }

  }

  trait DeserializerBuilder[Token, O] {
    val deserializer: Deserializer[Token, O]

    def andThen[O1, O2](other: Deserializer[Token, O1])(resultBinder: (O, O1) => O2) =
      DeserializerBuilder.andThen(deserializer, other)(resultBinder)

    def end: Deserializer[Token, O] =
      DeserializerBuilder.andThen[Token, O, Boolean, O](deserializer, DeserializerBuilder.isExhausted)((o, b) => o)

    def or(other: Deserializer[Token, O]): Deserializer[Token, O] =
      DeserializerBuilder.or(deserializer, other)

    def ||(other: Deserializer[Token, O]): Deserializer[Token, O] = or(other)
  }
}
