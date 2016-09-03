package com.pp.searchcriteria.core.search
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.{ BSONDocument, BSONDocumentReader, Macros }

/**
  * Created by pp on 8/7/16.
  */
trait CanSearchMongoCollection[A] {
  def find(collection: BSONCollection): Option[Vector[A]]
}

trait CanProduceMongoQuery {
  def mongoQuery: BSONDocument
}

object CanSearchMongoCollection {



  case class Person(name: String)

  def db1: reactivemongo.api.DefaultDB = ???
  val collection1: BSONCollection = db1.collection("people")
  val query1 =
    BSONDocument("age" -> BSONDocument("$gt" -> 25))

  collection1.find(query1).cursor[Person]().collect[Vector](100)

}