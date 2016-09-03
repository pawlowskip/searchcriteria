package com.pp.searchcriteria.common.domain

import com.pp.searchcriteria.core.SearchCriteria
import com.pp.searchcriteria.core.SearchCriteria.{Field, SCriteria}

/**
  * Created by pp on 8/16/16.
  */
case class Movie(id: Int,
                 title: String,
                 year: Int,
                 poster: Poster,
                 averageRating: Double,
                 viewers: Int,
                 description: String)

object Movie {

  case class MovieId(criteria: SCriteria[Int]) extends Field[Int, Movie](_.id)

  case class MovieTitle(criteria: SCriteria[String]) extends Field[String, Movie](_.title)

  case class MovieYear(criteria: SCriteria[Int]) extends Field[Int, Movie](_.year)

  case class MoviePoster(criteria: SCriteria[Poster]) extends Field[Poster, Movie](_.poster)

  case class MovieAverageRating(criteria: SCriteria[Double]) extends Field[Double, Movie](_.averageRating)

  case class MovieViewers(criteria: SCriteria[Int]) extends Field[Int, Movie](_.viewers)

  case class MovieDescription(criteria: SCriteria[String]) extends Field[String, Movie](_.description)

}
