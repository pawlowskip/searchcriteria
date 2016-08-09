package com.pp.searchcriteria.serialization

import com.pp.searchcriteria.core.SearchCriteria
import com.pp.searchcriteria.core.SearchCriteria.Field
import com.pp.searchcriteria.querystring.QueryString.{QSParam, _}
import com.pp.searchcriteria.serialization.Deserializer.DeserializerBuilder._
import com.pp.searchcriteria.serialization.Serialization.Reader

/**
  * Created by pp on 8/8/16.
  */
object SearchCriteriaDeserializationUtils {

  type Token = QSParam

  def multiValueDeserializer[T, Comb[T]](predicate: Token => Boolean,
                                         count: Token => Int,
                                         failMessage: String,
                                         fieldDeserializer: Deserializer[Token, SearchCriteria[T]],
                                         success: Seq[SearchCriteria[T]] => Comb[T],
                                         fail: Int => Deserializer[Token, Comb[T]]): Deserializer[Token, Comb[T]] =
    check[Token, Int](predicate, failMessage)(count)
      .flatMap {
        case i if i < 0 => fail(i)
        case i =>
          processTimes[Token, SearchCriteria[T]](fieldDeserializer, i) { // Can works without this always true
            case searchCriteria: Field[T, _] => true
            case _ => false
          }.map(success(_))
      }

  def singleValueDeserializer[T, Comb](predicate: Token => Boolean,
                                       failMessage: String,
                                       reader: Reader[T],
                                       transformer: T => Comb): Deserializer[Token, Comb] =
    for {
      value <- check[Token, T](predicate, failMessage)(t => reader.read(value(t)))
    } yield transformer(value)

}
