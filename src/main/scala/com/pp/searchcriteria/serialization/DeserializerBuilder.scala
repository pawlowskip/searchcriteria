package com.pp.searchcriteria.serialization

import com.pp.searchcriteria.core.SearchCriteria

/**
  * Created by pp on 8/15/16.
  */
object DeserializerBuilder {

  trait SearchCriteriaValidation[A, V] {
    val searchCriteria: SearchCriteria[A] with HasValue[V]
    def where(f: V => Boolean, failMessage: String = ""): SearchCriteria[A] = {
      searchCriteria.mapDeserializer { deserializer =>
        if (f(searchCriteria.getValue)) deserializer
        else Deserializer.failed(s"Validation error: $failMessage")
      }
    }

  }

  /**
    * Indicates possibility of getting value of type [T]
    */
  trait HasValue[T] {
    def getValue: T
  }

  /**
    * Placeholder method
    * @return null value as Type [A]
    */
  def * [A]: A = _

  implicit def toSearchCriteriaValidation[A, V](sc: SearchCriteria[A] with HasValue[V]): SearchCriteriaValidation[A, V] = {
    new SearchCriteriaValidation[A, V] { val searchCriteria = sc}
  }

  // helper methods for validation

  def betweenExl[T](from: T, to: T)(implicit ord: Ordering[T]): T => Boolean =
    t => ord.gt(t, from) && ord.lt(t, to)

  def betweenInl[T](from: T, to: T)(implicit ord: Ordering[T]): T => Boolean =
    t => ord.gteq(t, from) && ord.lteq(t, to)

  def tuple2[A, B](fa: A => Boolean, fb: B => Boolean): ((A, B)) => Boolean = {
    case (a, b) => fa(a) && fb(b)
  }

  def tuple2[A, B](f: (A, B) => Boolean): ((A, B)) => Boolean = {
    case (a, b) => f(a, b)
  }

  def tuple3[A, B, C](fa: A => Boolean, fb: B => Boolean, fc: C => Boolean): ((A, B, C)) => Boolean = {
    case (a, b, c) => fa(a) && fb(b) && fc(c)
  }

  def tuple3[A, B, C](f: (A, B, C) => Boolean): ((A, B, C)) => Boolean = {
    case (a, b, c) => f(a, b, c)
  }

  def option[A](f: A => Boolean)(allowNone: Boolean): Option[A] => Boolean = {
    case Some(a) => f(a)
    case None => allowNone
  }

  def some[A](f: A => Boolean): Option[A] => Boolean = {
    case Some(a) => f(a)
    case None => false
  }

  def none[A]: Option[A] => Boolean = {
    case None => true
    case _ => false
  }

  def and[T](fs: (T => Boolean)*): T => Boolean =
    t => fs.forall(fun => fun(t))


  def or[T](fs: (T => Boolean)*): T => Boolean =
    t => fs.exists(fun => fun(t))

}
