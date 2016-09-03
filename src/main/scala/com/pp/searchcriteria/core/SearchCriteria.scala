package com.pp.searchcriteria.core

import com.pp.searchcriteria.core.search.CanProduceMongoQuery
import com.pp.searchcriteria.querystring.QueryString
import com.pp.searchcriteria.querystring.QueryString._
import com.pp.searchcriteria.serialization.Serialization.{CanProduceQueryStringDeserializer, Reader, SerializableAsQS, Writer}
import com.pp.searchcriteria.serialization.Deserializer
import com.pp.searchcriteria.serialization.Deserializer.DeserializerOps
import com.pp.searchcriteria.serialization.Deserializer.DeserializerOps._
import com.pp.searchcriteria.serialization.DeserializationUtils._
import com.pp.searchcriteria.serialization.DeserializerBuilder.HasValue
import reactivemongo.bson.{BSONDocument, BSONValue, BSONWriter}

import scala.util.Random

/**
  *
  * @tparam A
  */
trait SearchCriteria[A, +Value]
  extends CanProduceQueryStringDeserializer[SearchCriteria[A, Value]] 
    with SerializableAsQS
    with HasValue[Value]
    with CanProduceMongoQuery { self =>
  //type QSDeserializer = Deserializer[QSParam, SearchCriteria[A, Value]]
  //def isSubCriteriaOf[B <: A](other: SearchCriteria[B]): Boolean
  def check(a: A): Boolean
  def identifier: String
//  def mapDeserializer(f: QSDeserializer => QSDeserializer): SearchCriteria[A, Value] = {
//    new SearchCriteria[A, Value] {
//      override def check(value: A) = self.check(value)
//      override def getDeserializer: Deserializer[QSParam, SearchCriteria[A, Value]] = f(self.getDeserializer)
//      override def toQueryString: QS = self.toQueryString
//      override def identifier: String = self.identifier
//      override def getValue: Value = self.getValue
//    }
//  }

}

object SearchCriteria {

  type SCriteria[A] = SearchCriteria[A, Any]

  def apply[A, Value](name: String,
                      checkFunction: A => Boolean,
                      value: Value,
                      toQueryStringFun: Seq[QSParam] = Seq(),
                      deserializer: Deserializer[QSParam, SearchCriteria[A, Value]] =
                             Deserializer.failed[QSParam, SearchCriteria[A, Value]]("Not implemented!")) = {

    new SearchCriteria[A, Value] {
      override def check(value: A) = checkFunction(value)

      override def getDeserializer: Deserializer[QSParam, SearchCriteria[A, Value]] = deserializer

      override def toQueryString: QS = toQueryStringFun

      override def identifier: String = name

      override def getValue: Value = value
    }
  }

  def create[A](criteria: SearchCriteria[A, Any])(implicit writer: Writer[A], reader: Reader[A]): Criteria[A, Any] =
    new Criteria[A, Any](Random.nextString(10), criteria, None)(writer, reader)

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
  case class Criteria[A, Value](identifier: String, criteria: SearchCriteria[A, Value], props: Option[SearchProps] = None)
                        (implicit writer: Writer[A], reader: Reader[A])

    extends SearchCriteria[A, Option[SearchProps]] {

    override def check(value: A): Boolean = criteria.check(value)

    private val defaultLimit = 10

    private def checkProps: Criteria[A, Value] = props match {
      case Some(SearchProps(-1, -1)) => this.copy(props = None)
      case _ => this
    }

    def limit(i: Int): Criteria[A, Value] = this.copy(props = Some(SearchProps(i, 0))).checkProps

    def page(p: Int): Criteria[A, Value] = props match {
      case None => this.copy(props = Some(SearchProps(defaultLimit, p)))
      case Some(SearchProps(l, _)) => this.copy(props = Some(SearchProps(l, p))).checkProps
      case _ => this.copy(props = Some(SearchProps(defaultLimit, p)))
    }

    def withName(name: String): Criteria[A, Value] = copy(identifier = name)

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

    override def getDeserializer: Deserializer[QSParam, Criteria[A, Value]] = {
      import QueryString.keyEqual
      type Token = QSParam
      type Header = (String, Int, Int)
      val limitDeserializer: Deserializer[Token, Int] =
        DeserializerOps.check[Token, Int](keyEqual("limit"), "Key should equal \"limit\"")(_._2.toInt)

      val pageDeserializer: Deserializer[Token, Int] =
        DeserializerOps.check[Token, Int](keyEqual("page"), "Key should equal \"page\"")(_._2.toInt)

      def headerDeserializer(criteriaName: String): Deserializer[Token, Header] =
        single("criteria" -> criteriaName, criteriaName)
          .andThen[Int, (String, Int)](limitDeserializer)((s, i) => (s, i))
          .andThen[Int, Header](pageDeserializer)((a: (String, Int), b: Int) => (a._1, a._2, b))

      headerDeserializer(identifier)
        .andThen[SearchCriteria[A, Value], Criteria[A, Value]](criteria.getDeserializer) { (header, searchCriteria) =>
          val (name, limit, page) = header
          Criteria[A, Value](identifier, searchCriteria).limit(limit).page(page)
        }
    }

    override def getValue: Option[SearchProps] = props
  }

  /**
    *
    * @param f
    * @tparam F
    * @tparam C
    */
  abstract class Field[F, C](f: C => F) extends SearchCriteria[C, Unit] { self =>

    val criteria: SearchCriteria[F, Any]

    override val identifier = s""""${getClass.getSimpleName}""""

    override def check(value: C): Boolean = criteria.check(f(value))

    override def toQueryString: Seq[QSParam] =
      Seq("field" -> identifier) ++ criteria.toQueryString

    override def getDeserializer: Deserializer[QSParam, SearchCriteria[C, Unit]] =
      single("field" -> identifier, Unit)
        .andThen[SearchCriteria[F, Any], SearchCriteria[C, Unit]](criteria.getDeserializer){(_, sf) =>
          new Field(f) {
            override val criteria: SearchCriteria[F, Any] = sf
            override def mongoQuery: BSONDocument = BSONDocument(self.identifier -> criteria.mongoQuery)
          }
        }

    override def getValue: Unit = Unit
  }

  /**
    *
    * @param criteria
    * @param writer
    * @param reader
    * @tparam A
    */
  case class And[A](criteria: SearchCriteria[A, Any]*)
                   (implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[A, Int] {

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
        criteria.map(_.getDeserializer),
        seq => And(seq: _*)
      )
    }

    override def identifier: String = "And"

    override def getValue: Int = criteria.length

    override def mongoQuery: BSONDocument = BSONDocument("$and" -> criteria.map(_.mongoQuery))
  }

//  object And {
//    def apply[A](criteria: SearchCriteria[A, Value]*)
//                (implicit writer: Writer[A], reader: Reader[A]): And[A] = new And(criteria)
//  }

  /**
    *
    * @param criteria
    * @param writer
    * @param reader
    * @tparam A
    */
  case class Or[A](criteria: SearchCriteria[A, Any]*)
                  (implicit writer: Writer[A], reader: Reader[A]) extends SearchCriteria[A, Int] {

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
        criteria.map(_.getDeserializer),
        seq => Or(seq: _*)
      )

    override def identifier: String = "Or"

    override def getValue: Int = criteria.length

    override def mongoQuery: BSONDocument = BSONDocument("$or" -> criteria.map(_.mongoQuery))
  }

//  object Or {
//    def apply[A](criteria: SearchCriteria[A, Value]*)
//                (implicit writer: Writer[A], reader: Reader[A]): Or[A] = new Or(criteria)
//  }

  /**
    *
    * @param criteria
    * @tparam A
    */
  case class Not[A](criteria: SearchCriteria[A, Any]) extends SearchCriteria[A, Unit] {

    override def check(value: A): Boolean = !criteria.check(value)

    override def toQueryString: Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      Seq(identifier -> "1")
      params ++= criteria.toQueryString
      params.toList
    }

    def getDeserializer: Deserializer[QSParam, Not[A]] =
      DeserializerOps
        .check[Token, Token](keyEqual(identifier), failMessage = s"Key should equal '$identifier'")(t => t)
        .flatMap { case t =>
            deserializeTimes[Token, SearchCriteria[A, Any]](criteria.getDeserializer, 1) {
              case _ => true
            }.map(seq => Not(seq.head))
        }

    override def identifier: String = "Not"

    override def getValue = Unit

    override def mongoQuery: BSONDocument = criteria match {
      case Equal(value) => BSONDocument("$ne" -> criteria.mongoQuery)
      case _ => BSONDocument("$not" -> criteria.mongoQuery)
    }
  }

  /**
    *
    * @param value
    * @param writer
    * @param reader
    * @tparam A
    */
  case class Equal[A](value: A)
                     (implicit writer: Writer[A], reader: Reader[A],
                      bsonWriter: Either[BSONWriter[A, BSONValue], BSONWriter[A, BSONDocument]]) extends SearchCriteria[A, A] {

    override def check(value: A): Boolean = this.value == value

    override def getDeserializer: Deserializer[QSParam, Equal[A]] =
      checkAndTransformDeserializer[A, Equal[A]](
        keyEqual(identifier), s"Key should equal '$identifier'", reader, Equal[A](_)
      )

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName

    override def getValue: A = value

    override def mongoQuery: BSONDocument = ???
  }

  case class OneOf[A](criteriaSeq: Seq[SearchCriteria[A, Any]], ifEmpty: Boolean) extends SearchCriteria[A, Any] {

    override def check(a: A): Boolean = criteriaSeq.headOption match {
      case None => ifEmpty
      case Some(c)  => c.check(a)
    }

    override def identifier: String = "OneOf"

    override def toQueryString: QS = criteriaSeq.headOption match {
      case None => Seq(identifier -> "isEmpty")
      case Some(c) => c.toQueryString
    }

    override def getDeserializer: Deserializer[(String, String), SearchCriteria[A, Any]] = {
      oneOf(criteriaSeq.map(_.getDeserializer))
    }

    override def getValue: Any = criteriaSeq.headOption match {
      case None => Unit
      case Some(c) => c.getValue
    }

    override def mongoQuery: BSONDocument = criteriaSeq.headOption match {
      case None => BSONDocument()
      case Some(c) => c.mongoQuery
    }
  }

  object OneOf {
    def apply[A](criteria: SearchCriteria[A, Any]*): OneOf[A] = {
      OneOf(criteria, ifEmpty = false)
    }
  }

  /**
    *
    * @tparam C
    * @tparam A
    */
  trait ContainsInvoker[C, A] {
    def contains(a: A): SearchCriteria[C, A]
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
                               writer: Writer[A], reader: Reader[A]) : SearchCriteria[C, A] = {

    containsInvoker.contains(value)
  }

  /**
    *
    */
  implicit val stringContainsInvoker: ContainsInvoker[String, String] =
    new ContainsInvoker[String, String] {
      override def contains(value: String): SearchCriteria[String, String] = StringContains(value)
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

    override def contains(value: Elem): SearchCriteria[Col, Elem] = SeqContains[Elem, Col](value)
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

    override def contains(value: Elem): SearchCriteria[Col, Elem] = SetContains[Elem, Col](value)
  }

  /**
    *
    * @param value
    */
  case class StringContains(value: String)
                           (implicit writer: Writer[String], reader: Reader[String])
    extends SearchCriteria[String, String] {

    override def check(value: String): Boolean = value.contains(this.value)

    override def getDeserializer: Deserializer[QSParam, StringContains] =
      checkAndTransformDeserializer[String, StringContains](keyEqual(identifier), s"Key should equal '$identifier'", reader, StringContains(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, value)

    override def identifier: String = getClass.getSimpleName

    override def getValue: String = value
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
                                        (implicit writer: Writer[A], reader: Reader[A])
    extends SearchCriteria[T, A] {

    override def check(value: T): Boolean = value.contains(this.value)

    override def getDeserializer: Deserializer[QSParam, SeqContains[A, T]] =
      checkAndTransformDeserializer[A, SeqContains[A, T]](keyEqual("SeqContains"), s"Key should equal '$identifier'", reader, SeqContains(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName

    override def getValue: A = value
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
                                        (implicit writer: Writer[A], reader: Reader[A])
    extends SearchCriteria[T, A] {

    override def check(value: T): Boolean = value.contains(this.value)

    override def getDeserializer: Deserializer[QSParam, SetContains[A, T]] =
      checkAndTransformDeserializer[A, SetContains[A, T]](keyEqual(identifier), s"Key should equal '$identifier'", reader, SetContains(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName

    override def getValue: A = value
  }

  /**
    *
    * @param value
    */
  case class MatchRegEx(value: String)(implicit writer: Writer[String], reader: Reader[String])
    extends SearchCriteria[String, String] {

    val regEx = value.r

    override def check(value: String): Boolean = regEx.findFirstIn(value).isDefined

    override def getDeserializer: Deserializer[QSParam, MatchRegEx] =
      checkAndTransformDeserializer[String, MatchRegEx](keyEqual(identifier), s"Key should equal '$identifier'", reader, MatchRegEx(_))

    override def toQueryString: QS = QueryString.fromPair(identifier, value)

    override def identifier: String = getClass.getSimpleName

    override def getValue: String = value
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
    extends SearchCriteria[N, N] {

    override def check(value: N): Boolean = ordering.lt(value, this.value)

    override def getDeserializer: Deserializer[QSParam, LessThan[N]] =
      checkAndTransformDeserializer[N, LessThan[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, LessThan[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName

    override def getValue: N = value
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
    extends SearchCriteria[N, N] {

    override def check(value: N): Boolean = ordering.lteq(value, this.value)

    override def getDeserializer: Deserializer[QSParam, LessOrEqual[N]] =
      checkAndTransformDeserializer[N, LessOrEqual[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, LessOrEqual[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName

    override def getValue: N = value
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
    extends SearchCriteria[N, N] {

    override def check(value: N): Boolean = ordering.gt(value, this.value)

    override def getDeserializer: Deserializer[QSParam, GreaterThan[N]] =
      checkAndTransformDeserializer[N, GreaterThan[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, GreaterThan[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName

    override def getValue: N = value
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
    extends SearchCriteria[N, N] {

    override def check(value: N): Boolean = ordering.gteq(value, this.value)

    override def getDeserializer: Deserializer[QSParam, GreaterOrEqual[N]] =
      checkAndTransformDeserializer[N, GreaterOrEqual[N]](keyEqual(identifier), s"Key should equal '$identifier'", reader, GreaterOrEqual[N](_))

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write(value))

    override def identifier: String = getClass.getSimpleName

    override def getValue: N = value
  }


  case class Between[N](from: N, to: N)(implicit ordering: Ordering[N], writer: Writer[(N, N)], reader: Reader[(N, N)])
    extends SearchCriteria[N, (N, N)] {

    override def check(value: N): Boolean = ordering.gt(value, from) && ordering.lt(value, to)

    override def getDeserializer: Deserializer[QSParam, Between[N]] =
      checkAndTransformDeserializer[(N, N), Between[N]](
        keyEqual(identifier),
        s"Key should equal '$identifier'",
        reader,
        pair => Between(pair._1, pair._2)
      )

    override def toQueryString: QS = QueryString.fromPair(identifier, writer.write((from, to)))

    override def identifier: String = getClass.getSimpleName

    override def getValue: (N, N) = (from, to)
  }

//  case class Optional[A](criteriaOpt: Option[SearchCriteria[A, Value]],
//                         withAbsence: Either[Boolean, SearchCriteria[A, Value]] = Left(true)) extends SearchCriteria[A, Value] {
//
//    lazy val checkFunction: A => Boolean = criteriaOpt match {
//      case Some(criteria) => a => criteria.check(a)
//      case None => withAbsence match {
//        case Left(b) => a => b
//        case Right(criteria) => a => criteria.check(a)
//      }
//    }
//
//    override def check(a: A): Boolean = checkFunction(a)
//
//    override def identifier: String = "Optional"
//
//    override def toQueryString: QS = ???
//
//    override def getDeserializer: Deserializer[(String, String), SearchCriteria[A, Value]] = ???
//  }

  /**
    *
    */
  object In {
    def apply[A](criteria: Seq[A])
                (implicit writer: Writer[A], reader: Reader[A]): SearchCriteria[A, Int] = Or(criteria.map(Equal(_)): _*)
  }

  /**
    *
    */
//  object Between{
//    def apply[A](from: A, to: A)
//                (implicit ordering: Ordering[A], writer: Writer[A], reader: Reader[A]) : And[A] =
//      And(
//        GreaterThan(from),
//        LessThan(to)
//      )
//  }

  /**
    *
    * @tparam T
    */
  trait NotEmptyInvoker[T]{
    def notEmpty: SearchCriteria[T, Unit]
  }

  /**
    *
    * @param notEmptyInvoker
    * @tparam T
    * @return
    */
  def NotEmpty[T](implicit notEmptyInvoker: NotEmptyInvoker[T]): SearchCriteria[T, Unit] = notEmptyInvoker.notEmpty

  /**
    *
    * @param notEmptyInvoker
    * @tparam T
    * @return
    */
  def IsEmpty[T](implicit notEmptyInvoker: NotEmptyInvoker[T]): SearchCriteria[T, Unit] = Not[T](notEmptyInvoker.notEmpty)


  /**
    *
    */
  implicit val nonEmptyString: NotEmptyInvoker[String] = new NotEmptyInvoker[String] {
    override def notEmpty: SearchCriteria[String, Unit] = NotEmptyString
  }

  /**
    *
    * @tparam T
    * @return
    */
  implicit def nonEmptyCollection[T <: Traversable[_]]: NotEmptyInvoker[T] = new NotEmptyInvoker[T] {
    override def notEmpty: SearchCriteria[T, Unit] = NotEmptyCollection()
  }

  /**
    *
    */
  case object NotEmptyString extends SearchCriteria[String, Unit] {

    override def check(value: String): Boolean = value.nonEmpty

    override def toQueryString: Seq[QSParam] =
      Seq(representation)

    override def getDeserializer: Deserializer[QSParam, SearchCriteria[String, Unit]] =
      DeserializerOps.single(representation, this)

    private val representation = identifier -> "1"

    override def identifier: String = getClass.getSimpleName

    override def getValue = Unit
  }

  /**
    *
    * @tparam T
    */
  case class NotEmptyCollection[T <: Traversable[_]]() extends SearchCriteria[T, Unit] {

    override def check(value: T): Boolean = value.nonEmpty

    override def toQueryString: Seq[QSParam] =
      Seq(representation)

    override def getDeserializer: Deserializer[QSParam, SearchCriteria[T, Unit]] =
      DeserializerOps.single(representation, this)

    private val representation = identifier -> "1"

    override def identifier: String = getClass.getSimpleName

    override def getValue = Unit
  }



}


