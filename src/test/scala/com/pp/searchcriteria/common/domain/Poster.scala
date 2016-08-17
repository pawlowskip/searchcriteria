package com.pp.searchcriteria.common.domain

import com.pp.searchcriteria.core.SearchCriteria
import com.pp.searchcriteria.core.SearchCriteria.{Field, SCriteria}

/**
  * Created by pp on 8/16/16.
  */
case class Poster(url: String)

object Poster{
  case class PosterUrl(criteria: SCriteria[String]) extends Field[String, Poster](_.url)
}
