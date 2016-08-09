package com.pp.searchcriteria.serialization

import com.pp.searchcriteria.serialization.Deserializer.DeserializerBuilder
import com.pp.searchcriteria.serialization.Serialization.Reader

/**
  * Created by pp on 8/8/16.
  */
object Deserializers {
  def fromReader[A](implicit reader: Reader[A]) = DeserializerBuilder.transform[String, A](reader.read)
}
