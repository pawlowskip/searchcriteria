package com.pp.searchcriteria.serialization

import com.pp.searchcriteria.querystring.QueryString
import com.pp.searchcriteria.querystring.QueryString.QS

/**
  * Created by pp on 8/8/16.
  */
object Serialization {

  trait Writer[A] {
    def write(a: A): String
  }

  trait Reader[A] {
    def read(s: String): A
  }

  implicit def fromUpickleWriter[T](implicit writer: upickle.default.Writer[T]): Writer[T] = {
    new Writer[T] {
      override def write(a: T): String = {
        upickle.default.write[T](a)
      }
    }
  }

  implicit def fromUpickleReader[T](implicit reader: upickle.default.Reader[T]): Reader[T] = {
    new Reader[T] {
      override def read(s: String): T = {
        upickle.default.read(s)
      }
    }
  }

  trait SerializableAsQS {
    def toQueryString: QS
  }

  trait SerializableAsXml {}

  trait SerializableAsJson {}

  trait CanProduceDeserializer[Token, A] {
    def getDeserializer: Deserializer[Token, A]
  }

  trait CanProduceQueryStringDeserializer[A]
    extends CanProduceDeserializer[QueryString.QSParam, A]

  trait CanProduceXmlDeserializer[A]

  trait CanProduceJsonDeserializer[A]

}
