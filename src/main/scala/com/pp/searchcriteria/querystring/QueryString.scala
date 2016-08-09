package com.pp.searchcriteria.querystring

/**
  * Created by pp on 8/7/16.
  */
object QueryString {
  type QSParam = (String, String)
  type QS = Iterable[QSParam]

  def key(qSParam: QSParam): String = qSParam._1

  def value(qSParam: QSParam): String = qSParam._2

  def fromPair(parameter: String, value: String): QS = Seq(parameter -> value)

  def keyEqual(k: String): QSParam => Boolean = _._1 == k

  def valueEqual(v: String): QSParam => Boolean = _._2 == v

}
