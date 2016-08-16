package com.pp.searchcriteria.serialization

import com.pp.searchcriteria.common.domain.Movie
import com.pp.searchcriteria.common.domain.Movie._
import com.pp.searchcriteria.common.domain.Poster.PosterUrl
import com.pp.searchcriteria.core.SearchCriteria.{And, Between, Equal, GreaterOrEqual, GreaterThan, NotEmptyString, Or, SearchProps, _}
import utest.TestSuite
import upickle.default._
import utest._
import com.pp.searchcriteria.serialization.Serialization._
import com.pp.searchcriteria.serialization.DeserializerBuilder._
import com.pp.searchcriteria.serialization.DeserializerBuilder.{*}

/**
  * Created by pp on 8/16/16.
  */
object DeserializerBuilderTest extends TestSuite {

  val tests = this {
    "Test [1] - ..." - {
      val deserializer =
        create[Movie](
          And(
            MovieTitle(
              Equal[String](*) where (_.startsWith("The "), "Title should starts with 'The '.")
            ),
            MovieId(
              GreaterOrEqual[Int](*) where (_ > 0, "Id should be greater or equal 0.")
            ),
            MovieYear(
              Between[Int](*, *) where (
                and(
                  tuple2(_ < _),
                  tuple2(_ > 1970, _ < 2009)
                ),
                failMessage = "'from' should be less than 'to', and ..."
                )
            ),
            MovieAverageRating(
              GreaterThan[Double](*) where betweenInl(0.0, 10.0)
            ),
            MovieDescription(
              Or(NotEmptyString, IsEmpty)
            ),
            MovieMyRating(Equal(*)),
            MovieViewers(
              GreaterThan[Int](*) where(_ > 0)
            ),
            MoviePoster(
              PosterUrl(
                Equal[String](*) where (_.startsWith("http://"))
              )
            )
          )
        ).withName("Movie")
         .where(
           option {case SearchProps(limit, page) => page > 0 && limit < 1000}(allowNone = true)
         )
         .getDeserializer

    }
  }

}
