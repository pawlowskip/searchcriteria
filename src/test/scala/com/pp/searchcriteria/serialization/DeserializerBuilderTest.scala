package com.pp.searchcriteria.serialization

import com.pp.searchcriteria.common.domain.{Movie, SampleData}
import com.pp.searchcriteria.common.domain.Movie._
import com.pp.searchcriteria.common.domain.Poster.PosterUrl
import com.pp.searchcriteria.core.SearchCriteria.{And, Between, Equal, GreaterOrEqual, GreaterThan, NotEmptyString, Or, SearchProps, _}
import utest.TestSuite
import upickle.default._
import utest._
import com.pp.searchcriteria.serialization.Serialization._
import com.pp.searchcriteria.serialization.DeserializerBuilder._
import com.pp.searchcriteria.serialization.DeserializerBuilder.*
import com.pp.searchcriteria.serialization.Deserializer._
import com.pp.searchcriteria.core.search.CanSearchCollection._

/**
  * Created by pp on 8/16/16.
  */
object DeserializerBuilderTest extends TestSuite {

  val tests = this {

    "Test [1] - Simple test" - {

      val deserializer = create[Movie](
        And(
          MovieTitle(
            Equal[String](*) where (! _.isEmpty, "Title should not be empty.")
          ),
          MovieId(
            GreaterOrEqual[Int](*) where (_ >= 0, "Id should be greater or equal 0.")
          )
        )
      ).withName("Movie").getDeserializer


      val criteria1 =
        create[Movie](
          And(
            MovieTitle(
              Equal("Movie title")
            ),
            MovieId(
              GreaterOrEqual(0)
            )
          )
        ).withName("Movie")

      val serializedQS1 = criteria1.toQueryString

      val deserializedQS1 = deserializer.deserialize(serializedQS1)

      //deserializedQS1 ==> Ok(criteria1, emptyInput, 10, Nil)

      deserializedQS1.get.filter(SampleData.movies) ==> criteria1.filter(SampleData.movies)

    }

    "Test [2] - ..." - {
      val deserializer =
        create[Movie](
          And(
            MovieTitle(
              Equal[String](*) where (_.charAt(0).isUpper, "Title should starts with Upper letter.")
            ),
            MovieId(
              GreaterOrEqual[Int](*) where (_ >= 0, "Id should be greater or equal 0.")
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
              OneOf(NotEmptyString, IsEmpty[String])
            ),
            MovieMyRating(Equal(*)),
            MovieViewers(
              GreaterThan[Int](*) where(_ > 0)
            ),
            MoviePoster(
              PosterUrl(
                Equal[String](*) where (_.startsWith("url"))
              )
            )
          )
        ).withName("Movie")
         .where(
           option[SearchProps]{case SearchProps(limit, page) => page >= 0 && limit < 1000}(allowNone = true)
         )
         .getDeserializer


      val criteria1 =
        create[Movie](
          And(
            MovieTitle(
              Equal("Movie title")
            ),
            MovieId(
              GreaterOrEqual(0)
            ),
            MovieYear(
              Between[Int](2000, 2005)
            ),
            MovieAverageRating(
              GreaterThan[Double](5.0)
            ),
            MovieDescription(
              NotEmptyString
            ),
            MovieViewers(
              GreaterThan[Int](1000)
            ),
            MoviePoster(
              PosterUrl(
                Equal[String]("url1")
              )
            )
          )
        ).withName("Movie").limit(100).page(0)

      val serializedQS1 = criteria1.toQueryString

      val deserializedQS1 = deserializer.deserialize(serializedQS1)

      //deserializedQS1 ==> Ok(criteria1, emptyInput, 10, Nil)

      deserializedQS1.get.filter(SampleData.movies) ==> criteria1.filter(SampleData.movies)


    }
  }

}
