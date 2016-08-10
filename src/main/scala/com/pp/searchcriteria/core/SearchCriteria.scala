package com.pp.searchcriteria.core

import com.pp.searchcriteria.querystring.QueryString
import com.pp.searchcriteria.querystring.QueryString._
import com.pp.searchcriteria.serialization.Serialization.{CanProduceQueryStringDeserializer, Reader, SerializableAsQS, Writer}
import com.pp.searchcriteria.serialization.Deserializer
import com.pp.searchcriteria.serialization.Deserializer.DeserializerBuilder
import com.pp.searchcriteria.serialization.Deserializer.DeserializerBuilder._
import com.pp.searchcriteria.serialization.SearchCriteriaDeserializationUtils._

import scala.util.Random

/**
  *
  * @tparam A
  */
trait SearchCriteria[A] extends CanProduceQueryStringDeserializer[SearchCriteria[A]] with SerializableAsQS {
  //def isSubCriteriaOf[B <: A](other: SearchCriteria[B]): Boolean
  def check(a: A): Boolean
  def identifier: String
}

object SearchCriteria {

  def apply[A](name: String,
               checkFunction: A => Boolean,
               toQueryStringFun: Seq[QSParam] = Seq(),
               deserializer: Deserializer[QSParam, SearchCriteria[A]] =
                             Deserializer.failed[QSParam, SearchCriteria[A]]("Not implemented!")) = {

    new SearchCriteria[A] {
      override def check(value: A) = checkFunction(value)

      override def getDeserializer: Deserializer[QSParam, SearchCriteria[A]] = deserializer

      override def toQueryString: QS = toQueryStringFun

      override def identifier: String = name
    }
  }

  def create[A](criteria: SearchCriteria[A])(implicit writer: Writer[A], reader: Reader[A]) =
    new Criteria[A](Random.nextString(10), criteria, None)(writer, reader)

  /**
    *
    * @param limit
    * @param page
    */
  case class SearchProps(limit: Int, page: Int)

  /**
    *
    * @param identifier
    * @param criteria
    * @param props
    * @param writer
    * @param reader
    * @tparam A
    */
  case class Criteria[A](identifier: String, criteria: SearchCriteria[A], props: Option[SearchProps] = None)
                        (implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[A] {

    override def check(value: A): Boolean = criteria.check(value)

    private val defaultLimit = 10

    private def checkProps: Criteria[A] = props match {
      case Some(SearchProps(-1, -1)) => this.copy(props = None)
      case _ => this
    }

    def limit(i: Int): Criteria[A] = this.copy(props = Some(SearchProps(i, 0))).checkProps

    def page(p: Int): Criteria[A] = props match {
      case None => this.copy(props = Some(SearchProps(defaultLimit, p)))
      case Some(SearchProps(l, _)) => this.copy(props = Some(SearchProps(l, p))).checkProps
      case _ => this.copy(props = Some(SearchProps(defaultLimit, p)))
    }

    def withName(name: String): Criteria[A] = copy(identifier = name)

    override def toQueryString: Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      params += "criteria" -> s"""$identifier"""
      props match {
        case Some(SearchProps(limit, page)) =>
          params += "limit" -> limit.toString
          params += "page" -> page.toString
        case None =>
          params += "limit" -> "-1"
          params += "page" -> "-1"
      }
      params ++= criteria.toQueryString
      params.toList
    }

    override def getDeserializer: Deserializer[QSParam, SearchCriteria[A]] = {
      import QueryString.keyEqual
      type Token = QSParam
      type Header = (String, Int, Int)
      val limitDeserializer: Deserializer[Token, Int] =
        DeserializerBuilder.check[Token, Int](keyEqual("limit"), "Key should equal \"limit\"")(_._2.toInt)

      val pageDeserializer: Deserializer[Token, Int] =
        DeserializerBuilder.check[Token, Int](keyEqual("page"), "Key should equal \"page\"")(_._2.toInt)

      def headerDeserializer(criteriaName: String): Deserializer[Token, Header] =
        single("criteria" -> criteriaName, criteriaName)
          .andThen[Int, (String, Int)](limitDeserializer)((s, i) => (s, i))
          .andThen[Int, Header](pageDeserializer)((a: (String, Int), b: Int) => (a._1, a._2, b))

      headerDeserializer(identifier)
        .andThen[SearchCriteria[A], Criteria[A]](criteria.getDeserializer) { (header, searchCriteria) =>
          val (name, limit, page) = header
          Criteria[A](identifier, searchCriteria).limit(limit).page(page)
        }
    }
  }

  /**
    *
    * @param f
    * @param writer
    * @param reader
    * @tparam F
    * @tparam C
    */
  abstract class Field[F, C](f: C => F)(implicit writer: Writer[C], reader: Reader[C]) extends SearchCriteria[C] {

    val criteria: SearchCriteria[F]

    override val identifier = getClass.getSimpleName

    override def check(value: C): Boolean = criteria.check(f(value))

    override def toQueryString: Seq[QSParam] =
      Seq("field" -> s""""$identifier"""") ++ criteria.toQueryString

    override def getDeserializer: Deserializer[QSParam, SearchCriteria[C]] =
      single("field" -> identifier, Unit)
        .andThen[SearchCriteria[F], SearchCriteria[C]](criteria.getDeserializer){(_, sf) =>
          new Field(f) {
            override val criteria: SearchCriteria[F] = sf
          }
        }
  }

  /**
    *
    * @param criteria
    * @param writer
    * @param reader
    * @tparam A
    */
  case class And[A](criteria: Seq[SearchCriteria[A]])
                   (implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[A] {

    override def check(value: A): Boolean = criteria.forall(_.check(value))

    override def toQueryString: Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      params += identifier -> criteria.size.toString
      criteria.foreach(c => params ++= c.toQueryString)
      params.toList
    }

    override def getDeserializer: Deserializer[QSParam, And[A]] = {


      multiValueDeserializer(
        keyEqual(identifier),
        _._2.toInt,
        s"Value for param '$identifier' should be convertable to Int.",
        oneOf(criteria.map(_.getDeserializer)),
        seq => And(seq),
        i => Deserializer.failed[Token, And[A]](s"And should contain positive number of criteria (passed $i).")
      )
    }


    override def identifier: String = "And"
  }

  object And {
    def apply[A](criteria: SearchCriteria[A]*)
                (implicit writer: Writer[A], reader: Reader[A]): SearchCriteria[A] = And(criteria)
  }

  /**
    *
    * @param criteria
    * @param writer
    * @param reader
    * @tparam A
    */
  case class Or[A](criteria: Seq[SearchCriteria[A]])
                  (implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[A] {

    override def check(value: A): Boolean = criteria.exists(_.check(value))

    override def toQueryString: Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      params += identifier -> criteria.size.toString
      criteria.foreach(c => params ++= c.toQueryString)
      params.toList
    }

    override def getDeserializer: Deserializer[QSParam, Or[A]] =
      multiValueDeserializer(
        keyEqual(identifier),
        _._2.toInt,
        s"Value for param '$identifier' should be convertable to Int.",
        oneOf(criteria.map(_.getDeserializer)),
        seq => Or(seq),
        i => Deserializer.failed[Token, Or[A]](s"Or should contain positive number of criteria (passed $i).")
      )

    override def identifier: String = "Or"

  }

  object Or {
    def apply[A](criteria: SearchCriteria[A]*)
                (implicit writer: Writer[A], reader: Reader[A]): SearchCriteria[A] = Or(criteria)
  }

  /**
    *
    * @param criteria
    * @tparam A
    */
  case class Not[A](criteria: SearchCriteria[A]) extends SearchCriteria[A] {

    override def check(value: A): Boolean = !criteria.check(value)

    override def toQueryString: Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      Seq(identifier -> "1")
      params ++= criteria.toQueryString
      params.toList
    }

    def getDeserializer: Deserializer[QSParam, Not[A]] =
      DeserializerBuilder
        .check[Token, Token](keyEqual(identifier), failMessage = s"Key should equal '$identifier'")(t => t)
        .flatMap { case t =>
            deserializeTimes[Token, SearchCriteria[A]](criteria.getDeserializer, 1) {
              case _ => true
            }.map(seq => Not(seq.head))
        }

    override def identifier: String = "Not"
  }

  /**
    *
    * @param value
    * @param writer
    * @param reader
    * @tparam A
    */
  case class Equal[A](value: A)(implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[A] {

    override def check(value: A): Boolean = this.value == value

    override def getDeserializer: Deserializer[QSParam, Equal[A]] =
      singleValueDeserializer[A, Equal[A]](keyEqual(identifier), s"Key should equal '$identifier'", reader, Equal[A](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @tparam C
    * @tparam A
    */
  trait ContainsInvoker[C, A] {
    def contains(a: A): SearchCriteria[C]
  }

  /**
    *
    * @param value
    * @param containsInvoker
    * @param writer
    * @param reader
    * @tparam C
    * @tparam A
    * @return
    */
  def Contains[C, A](value: A)(implicit containsInvoker: ContainsInvoker[C, A],
                               writer: Writer[A], reader: Reader[A]) : SearchCriteria[C] = {

    containsInvoker.contains(value)
  }

  /**
    *
    */
  implicit val stringContainsInvoker: ContainsInvoker[String, String] = new ContainsInvoker[String, String] {
    override def contains(value: String): SearchCriteria[String] = StringContains(value)
  }

  /**
    *
    * @param writer
    * @param reader
    * @tparam Elem
    * @tparam Col
    * @return
    */
  implicit def seqContainsInvoker[Elem, Col <: Seq[Elem]](implicit writer: Writer[Elem], reader: Reader[Elem])
  : ContainsInvoker[Col, Elem] = new ContainsInvoker[Col, Elem] {

    override def contains(value: Elem): SearchCriteria[Col] = SeqContains[Elem, Col](value)
  }

  /**
    *
    * @param writer
    * @param reader
    * @tparam Elem
    * @tparam Col
    * @return
    */
  implicit def setContainsInvoker[Elem, Col <: Set[Elem]]
  (implicit writer: Writer[Elem], reader: Reader[Elem]) : ContainsInvoker[Col, Elem] = new ContainsInvoker[Col, Elem] {

    override def contains(value: Elem): SearchCriteria[Col] = SetContains[Elem, Col](value)
  }

  /**
    *
    * @param value
    */
  case class StringContains(value: String)
                           (implicit writer: Writer[String], reader: Reader[String]) extends SearchCriteria[String] {

    override def check(value: String): Boolean = value.contains(this.value)

    override def getDeserializer: Deserializer[QSParam, StringContains] =
      singleValueDeserializer[String, StringContains](keyEqual(identifier), s"Key should equal '$identifier'", reader, StringContains(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, value)

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @param value
    * @param writer
    * @param reader
    * @tparam A
    * @tparam T
    */
  case class SeqContains[A, T <: Seq[A]](value: A)
                                        (implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[T] {

    override def check(value: T): Boolean = value.contains(this.value)

    override def getDeserializer: Deserializer[QSParam, SeqContains[A, T]] =
      singleValueDeserializer[A, SeqContains[A, T]](keyEqual("SeqContains"), s"Key should equal '$identifier'", reader, SeqContains(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @param value
    * @param writer
    * @param reader
    * @tparam A
    * @tparam T
    */
  case class SetContains[A, T <: Set[A]](value: A)
                                        (implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[T] {

    override def check(value: T): Boolean = value.contains(this.value)

    override def getDeserializer: Deserializer[QSParam, SetContains[A, T]] =
      singleValueDeserializer[A, SetContains[A, T]](keyEqual(identifier), s"Key should equal '$identifier'", reader, SetContains(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @param value
    */
  case class MatchRegEx(value: String)(implicit writer: Writer[String], reader: Reader[String]) extends SearchCriteria[String] {

    val regEx = value.r

    override def check(value: String): Boolean = regEx.findFirstIn(value).isDefined

    override def getDeserializer: Deserializer[QSParam, MatchRegEx] =
      singleValueDeserializer[String, MatchRegEx](keyEqual(identifier), s"Key should equal '$identifier'", reader, MatchRegEx(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, value)

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @param value
    * @param ordering
    * @param writer
    * @param reader
    * @tparam N
    */
  case class LessThan[N](value: N)(implicit ordering: Ordering[N], writer: Writer[N], reader: Reader[N])
    extends SearchCriteria[N] {

    override def check(value: N): Boolean = ordering.lt(value, this.value)

    override def getDeserializer: Deserializer[QSParam, LessThan[N]] =
      singleValueDeserializer[N, LessThan[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, LessThan[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @param value
    * @param ordering
    * @param writer
    * @param reader
    * @tparam N
    */
  case class LessOrEqual[N](value: N)(implicit ordering: Ordering[N], writer: Writer[N], reader: Reader[N])
    extends SearchCriteria[N] {

    override def check(value: N): Boolean = ordering.lteq(value, this.value)

    override def getDeserializer: Deserializer[QSParam, LessOrEqual[N]] =
      singleValueDeserializer[N, LessOrEqual[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, LessOrEqual[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @param value
    * @param ordering
    * @param writer
    * @param reader
    * @tparam N
    */
  case class GreaterThan[N](value: N)(implicit ordering: Ordering[N], writer: Writer[N], reader: Reader[N])
    extends SearchCriteria[N] {

    override def check(value: N): Boolean = ordering.gt(value, this.value)

    override def getDeserializer: Deserializer[QSParam, GreaterThan[N]] =
      singleValueDeserializer[N, GreaterThan[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, GreaterThan[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @param value
    * @param ordering
    * @param writer
    * @param reader
    * @tparam N
    */
  case class GreaterOrEqual[N](value: N)(implicit ordering: Ordering[N], writer: Writer[N], reader: Reader[N])
    extends SearchCriteria[N] {

    override def check(value: N): Boolean = ordering.gteq(value, this.value)

    override def getDeserializer: Deserializer[QSParam, GreaterOrEqual[N]] =
      singleValueDeserializer[N, GreaterOrEqual[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, GreaterOrEqual[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    */
  object In {
    def apply[A](criteria: Seq[A])
                (implicit writer: Writer[A], reader: Reader[A]): SearchCriteria[A] = Or(criteria.map(Equal(_)): _*)
  }

  /**
    *
    */
  object Between{
    def apply[A](from: A, to: A)
                (implicit ordering: Ordering[A], writer: Writer[A], reader: Reader[A]) : SearchCriteria[A] =
      And(
        GreaterThan(from),
        LessThan(to)
      )
  }

  /**
    *
    * @tparam T
    */
  trait NotEmptyInvoker[T]{
    def notEmpty: SearchCriteria[T]
  }

  /**
    *
    * @param notEmptyInvoker
    * @tparam T
    * @return
    */
  def NotEmpty[T](implicit notEmptyInvoker: NotEmptyInvoker[T]): SearchCriteria[T] = notEmptyInvoker.notEmpty

  /**
    *
    * @param notEmptyInvoker
    * @tparam T
    * @return
    */
  def IsEmpty[T](implicit notEmptyInvoker: NotEmptyInvoker[T]): SearchCriteria[T] = Not(notEmptyInvoker.notEmpty)


  /**
    *
    */
  implicit val nonEmptyString: NotEmptyInvoker[String] = new NotEmptyInvoker[String] {
    override def notEmpty: SearchCriteria[String] = NotEmptyString
  }

  /**
    *
    * @tparam T
    * @return
    */
  implicit def nonEmptyCollection[T <: Traversable[_]]: NotEmptyInvoker[T] = new NotEmptyInvoker[T] {
    override def notEmpty: SearchCriteria[T] = NotEmptyCollection()
  }

  /**
    *
    */
  case object NotEmptyString extends SearchCriteria[String] {

    override def check(value: String): Boolean = value.nonEmpty

    override def toQueryString: Seq[QSParam] =
      Seq(representation)

    override def getDeserializer: Deserializer[QSParam, SearchCriteria[String]] =
      DeserializerBuilder.single(representation, this)

    private val representation = identifier -> "1"

    override def identifier: String = getClass.getSimpleName
  }

  /**
    *
    * @tparam T
    */
  case class NotEmptyCollection[T <: Traversable[_]]() extends SearchCriteria[T] {

    override def check(value: T): Boolean = value.nonEmpty

    override def toQueryString: Seq[QSParam] =
      Seq(representation)

    override def getDeserializer: Deserializer[QSParam, SearchCriteria[T]] =
      DeserializerBuilder.single(representation, this)

    private val representation = identifier -> "1"

    override def identifier: String = getClass.getSimpleName
  }



}


