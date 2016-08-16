package com.pp.searchcriteria.common.domain

import com.pp.searchcriteria.core.SearchCriteria
import com.pp.searchcriteria.core.SearchCriteria.Field

/**
  * Created by pp on 8/16/16.
  */
case class Movie(id: Int,
                 title: String,
                 year: Int,
                 poster: Poster,
                 averageRating: Double,
                 myRating: Option[Int],
                 viewers: Int,
                 description: String)

object Movie {

  case class MovieId(criteria: SearchCriteria[Int]) extends Field[Int, Movie](_.id)

  case class MovieTitle(criteria: SearchCriteria[String]) extends Field[String, Movie](_.title)

  case class MovieYear(criteria: SearchCriteria[Int]) extends Field[Int, Movie](_.year)

  case class MoviePoster(criteria: SearchCriteria[Poster]) extends Field[Poster, Movie](_.poster)

  case class MovieAverageRating(criteria: SearchCriteria[Double]) extends Field[Double, Movie](_.averageRating)

  case class MovieMyRating(criteria: SearchCriteria[Option[Int]]) extends Field[Option[Int], Movie](_.myRating)

  case class MovieViewers(criteria: SearchCriteria[Int]) extends Field[Int, Movie](_.viewers)

  case class MovieDescription(criteria: SearchCriteria[String]) extends Field[String, Movie](_.description)

}
