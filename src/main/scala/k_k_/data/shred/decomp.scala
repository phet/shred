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

package k_k_.data.shred

package decomp {

import scala.util.matching.Regex


sealed abstract class Delimiter
case class Static_Delim(chars: String) extends Delimiter
case class Regex_Delim(regex: Regex) extends Delimiter


sealed abstract class Structure

//!!!need to handle delimiter v. separator!!!!

case class Field_Delimited(
  field: Delimiter,
  record: Delimiter
  ) extends Structure

//???how to seperately specify field escape v. record escape???
case class Escapable_Field_Delimited(
  field: Delimiter,
  record: Delimiter,
  escape: Static_Delim
  //???perhaps should also take an 'escape function'???
  ) extends Structure

case class Quotable_Field_Delimited(
  field: Delimiter,
  record: Delimiter,
  quote: Delimiter,
  end_quote: String => Delimiter
  ) extends Structure

case class Quoted_Field_Delimited(
  field: Delimiter,
  record: Delimiter,
  quote: Delimiter,
  end_quote: String => Delimiter
  ) extends Structure


}
