/*
   shred - The Streaming Hierarical Record Editor

   Copyright (c)2012 by Corbin "Kip" Kohn
   All Rights Reserved.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
package k_k_.data.shred.decomp

import scala.util.matching.Regex


object Delim {
  def apply(c: Char)  : Delim = new Static_Delim(c.toString)
  def apply(s: String): Delim = new Static_Delim(s)
  def apply(re: Regex): Delim = new Regex_Delim(re)

  implicit def fromChar(c: Char)    : Delim = apply(c)
  implicit def fromString(s: String): Delim = apply(s)
  implicit def fromRegex(re: Regex) : Delim = apply(re)
}

sealed abstract class Delim
case class Static_Delim(str: String) extends Delim
case class Regex_Delim(re: Regex) extends Delim


sealed abstract class Format

//!!!need to handle delimiter separated v. terminated!!!!
//!!!whether or not to trim surrounding spaces shall be a function one could apply after parsing!!!!

//???skip empty fields???
//???skip empty records???

// NOTE: 'field' is a synonym for 'sub-record'/'child record'

case class Field_Delimited(
    field: Delim,
    record: Delim
  ) extends Format

//???how to seperately specify field escape if distinct from record escape???
case class Escapable_Field_Delimited(
    field: Delim,
    record: Delim,
    escape: Static_Delim
    //???perhaps also should take an 'escape function'???
    //--NO: an escape pertains to exactly one succeeding char only
  ) extends Format

case class Quotable_Field_Delimited(
    field: Delim,
    record: Delim,
    quote: Delim,
    end_quote: String => Delim // called with whatever quote matched
  ) extends Format

case class Quoted_Field_Delimited(
    field: Delim,
    record: Delim,
    quote: Delim,
    end_quote: String => Delim // called with whatever quote matched
  ) extends Format


object Field {

  def apply(content: String, post_delim: String) =
    new Field(content, None, Some(post_delim))

  def apply(content: String, pre_delim: String, post_delim: String) =
    new Field(content, Some(pre_delim), Some(post_delim))
}

final class Field(
    val content: String,
    val pre_delim: Option[String],
    val post_delim: Option[String]
  ) {

  override def toString =
    "Field('%s' + [%s] + '%s')".format(
        pre_delim.getOrElse(""), content, post_delim.getOrElse(""))
}

object Record_Parser {

  def apply(fmt: Format): Record_Parser =
    fmt match {
      case Field_Delimited(field_delim, record_delim) =>
        new Field_Delimited_Parser(field_delim, record_delim)
    }
}


import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader

case class Parse_Exception(msg: String, pos: Reader[Char]) extends RuntimeException

sealed abstract class Record_Parser extends RegexParsers {

  final def parse_stream(r: Reader[Char]): Stream[Seq[String]] = {
    def parse_next(in: Input): Stream[Seq[String]] =
      parse(in) match {
        case Success(result, remaining) =>
          Stream.cons(result, parse_next(remaining))
        case NoSuccess(_, remaining) if remaining.atEnd =>
          Stream.empty
        case NoSuccess(msg, remaining) =>
          throw Parse_Exception(msg, remaining)
      }
    parse_next(r)
  }


  protected def parse(in: Input): ParseResult[List[String]]


  implicit protected def toParser(delim: Delim): Parser[String] =
    delim match {
      case Static_Delim(str) => str
      case Regex_Delim(re) => re
    }
}

class Field_Delimited_Parser(
    field_spec: Delim,
    record_spec: Delim
  ) extends Record_Parser {

  override def skipWhitespace = false


  override protected def parse(in: Input): ParseResult[List[String]] =
    parse(record, in)

  protected def field_delim: Parser[String] = field_spec
  protected def record_delim: Parser[String] = record_spec

  protected def ANY_CHAR = ".".r

  protected def record: Parser[List[String]] =
    repsep(field, field_delim) <~ (record_delim)

  protected def field: Parser[String] =
    (content*) ^^ { case ls => ls.mkString("") }

  // NOTE: `not` consumes no input
  protected def content =
    not(record_delim) ~> not(field_delim) ~> ANY_CHAR

/*
  def field: Parser[String] = escaped | nonescaped


  def escaped: Parser[String] = {
    ((SPACES?)~>DQUOTE~>((TXT|COMMA|CRLF|DQUOTE2)*)<~DQUOTE<~(SPACES?)) ^^ { 
      case ls => ls.mkString("")
    }
  }

  def nonescaped: Parser[String] = (content*) ^^ { case ls => ls.mkString("") }

if no record
  if no field
    get char -- repeat
  else
    get field
else get record
*/
}
