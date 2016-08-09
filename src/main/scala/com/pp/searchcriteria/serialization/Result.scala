package com.pp.searchcriteria.serialization

/**
  * Created by pp on 8/8/16.
  */
trait Result[+A, Token] {
  val inputLeft: Deserializer.Input[Token]
  def tokensParsed: Int
  def map[B](f: A => B): Result[B, Token]
  def flatMap[B](f: A => Result[B, Token]): Result[B, Token]
  def withTokensParsed(i: Int => Int): Result[A, Token]
  def get: A
  val messageStack: List[String]
  def withFailMessage(message: String): Result[A, Token]
  def withStackMessage(stackF: List[String] => List[String]): Result[A, Token]
}

case class Ok[+A, Token](result: A, inputLeft: Deserializer.Input[Token], tokensParsed: Int, messageStack: List[String])
  extends Result[A, Token] {

  override def map[B](f: A => B): Result[B, Token] = Ok(f(result), inputLeft, tokensParsed, messageStack)

  override def flatMap[B](f: A => Result[B, Token]): Result[B, Token] =
    f(result).withTokensParsed(tokensParsed + _)

  override def withTokensParsed(i: Int => Int): Result[A, Token] =
    copy(tokensParsed = i(tokensParsed))

  override def get: A = result

  override def withFailMessage(message: String): Ok[A, Token] = copy(messageStack = message :: messageStack)

  override def withStackMessage(stackF: (List[String]) => List[String]): Result[A, Token] =
    this.copy(messageStack = stackF(messageStack))
}

case class Fail[Token](cause: String, inputLeft: Deserializer.Input[Token], messageStack: List[String])
  extends Result[Nothing, Token] {

  override def map[B](f: Nothing => B): Result[B, Token] = this

  override def flatMap[B](f: Nothing => Result[B, Token]): Result[B, Token] = this

  override def withTokensParsed(i: Int => Int): Result[Nothing, Token] = this

  override val tokensParsed: Int = 0

  override def get: Nothing = throw new IllegalStateException("Fail!")

  override def withFailMessage(message: String): Result[Nothing, Token] =
    this.copy(messageStack = message :: messageStack)

  override def withStackMessage(stackF: (List[String]) => List[String]): Fail[Token] =
    this.copy(messageStack = stackF(messageStack))
}
