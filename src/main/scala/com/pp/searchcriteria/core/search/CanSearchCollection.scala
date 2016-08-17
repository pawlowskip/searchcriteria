package com.pp.searchcriteria.core.search

import com.pp.searchcriteria.core.SearchCriteria
import com.pp.searchcriteria.core.SearchCriteria.{Criteria, SearchProps}

/**
  * Created by pp on 8/7/16.
  */
trait CanSearchCollection[A] {
  def filter[C <: Iterable[A]](collection: C): Iterable[A]
}

object CanSearchCollection {

  class FilterExecutor[A](criteria: SearchCriteria[A, Any]) extends  CanSearchCollection[A] {
    override def filter[C <: Iterable[A]](collection: C): Iterable[A] = criteria match {
      case Criteria(_, searchCriteria, None) => collection.filter(a => searchCriteria.check(a))
      case Criteria(_, searchCriteria, Some(SearchProps(limit, page))) =>
        collection.filter(a => searchCriteria.check(a)).slice(page * limit, (page + 1) * limit)
      case _ => collection.filter(p => criteria.check(p))
    }
  }

  implicit def fromSearchCriteria[A](searchCriteria: SearchCriteria[A, Any]): CanSearchCollection[A] = {
    new FilterExecutor[A](searchCriteria)
  }
}
