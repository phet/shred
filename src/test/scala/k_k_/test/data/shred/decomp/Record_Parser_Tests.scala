package k_k_.test.data.shred.decomp

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scala.util.parsing.input.{CharSequenceReader, StreamReader}

import k_k_.data.shred.decomp._


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class Record_Parser_Tests extends FunSuite with ShouldMatchers {

  test("Field_Delimited parser") {
    val parser = Record_Parser(Field_Delimited(',', "\n"))
    val test_str = """
a,b, c , d
1,2,56,,23

,just,tell,,me,s-
thing
"""
    val expected = Seq(
        Seq(""),
        Seq("a", "b", " c ", " d"),
        Seq("1", "2", "56", "", "23"),
        Seq(""),
        Seq("", "just", "tell", "", "me", "s-"),
        Seq("thing")
      )
    val results = parser.parse_stream(new CharSequenceReader(test_str))
    stream_should_equal_seq(results, expected)
  }


  protected def stream_should_equal_seq[T](stream: Stream[T], expect: Seq[T]) {
    val max_lookahead = 100
    def desc_more_len(seq: Seq[T]): String = {
      val n_more = seq.take(max_lookahead).size
      if (n_more == max_lookahead) "at least %d".format(n_more)
      else n_more.toString
    }
    def validate_remaining(rest: Stream[T], remaining: Seq[T], i: Int = 0) {
      (rest.isEmpty, remaining.isEmpty) match {
        case (false, false) =>
          (i, rest.head) should be ((i, remaining.head))
          validate_remaining(rest.tail, remaining.tail, i + 1)
        case (true, true) => // success!
        case (true, false) =>
          val more_len = desc_more_len(remaining)
          sys.error(
              ("Stream shorter than expect (final index: %d), while seq " +
               "has %s elements remaining, beginning with [%s]").format(
                  i, more_len, rest.head))
        case (false, true) =>
          val more_len = desc_more_len(rest)
          sys.error(
              ("Stream longer than expect (from index: %d), with %s " +
               "elements remaining, beginning with [%s]").format(
                  i, more_len, remaining.head))
      }
    }
    validate_remaining(stream, expect)
  }
}
