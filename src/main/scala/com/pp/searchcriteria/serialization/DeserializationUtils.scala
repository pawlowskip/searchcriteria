package com.pp.searchcriteria.serialization

import com.pp.searchcriteria.core.SearchCriteria
import com.pp.searchcriteria.core.SearchCriteria.Field
import com.pp.searchcriteria.querystring.QueryString.{QSParam, _}
import com.pp.searchcriteria.serialization.Deserializer.DeserializerOps._
import com.pp.searchcriteria.serialization.Serialization.Reader

import scala.util.{Try, Success, Failure}

/**
  * Created by pp on 8/8/16.
  */
object DeserializationUtils {

  type Token = QSParam

  def multiValueDeserializer[T, Comb[T]](firstTokenPredicate: Token => Boolean,
                                         countFromFirstToken: Token => Int,
                                         failMessage: String,
                                         possibleDeserializers: Seq[Deserializer[Token, SearchCriteria[T]]],
                                         success: Seq[SearchCriteria[T]] => Comb[T]): Deserializer[Token, Comb[T]] =
    check[Token, Int](firstTokenPredicate, failMessage)(countFromFirstToken)
      .flatMap {
        case i if i < 0 => Deserializer.failed(s"Should contain positive number of required deserializations (passed $i).")
        case i =>
          deserializeTimes[Token, SearchCriteria[T]](oneOf(possibleDeserializers), i) { // Can works without this always true
            case _  => true
          }.map(success(_))
      }

  def checkAndTransformDeserializer[T, Comb](predicate: Token => Boolean,
                                             failMessage: String,
                                             reader: Reader[T],
                                             transformer: T => Comb): Deserializer[Token, Comb] =
    for {
      value <- check[Token, T](predicate, failMessage)(t => reader.read(value(t)))
    } yield transformer(value)

}
