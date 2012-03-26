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


sealed abstract class Indicator
sealed abstract class Offset extends Indicator {
  def into[T](seq: Seq[T]): Option[T] =
    this match {
      case Forward_Offset(i) => seq.lift(i)
      case Reverse_Offset(i) => seq.lift(seq.size - i)
    }
}
case class Forward_Offset(i: Int) extends Offset { assert(i >= 0) }
case class Reverse_Offset(i: Int) extends Offset { assert(i >  0) }
case class Name(ident: String) extends Indicator


//????all Addresses are assumed relative; how to specify absolute Address--or is that done at another layer???
case class Address(chain: List[Indicator]) {
  def ::(hd: Indicator) = Address(hd :: chain)
}

object Address_Point {
  def apply(addr: Address) = new Address_Point(addr)
}
/** an `Address.chain` 'broken' around a single point of interest */
case class Address_Point(
    pred_chain: List[Indicator],
    point: Indicator,
    succ_chain: List[Indicator]
  ) {
  /** point at the beginning of `Address`, which must not have chain == Nil */
  def this(addr: Address) =
    this(Nil, addr.chain.head, addr.chain.tail)

  def ::(hd: Indicator) =
    copy(pred_chain = hd :: pred_chain)
}

sealed abstract class Ref {
  def ::(hd: Indicator): Ref
}
case class Alias(addr: Address) extends Ref {
  def ::(hd: Indicator) = Alias(hd :: addr)
}
case class Ambiguity(points: Seq[Address_Point]) extends Ref {
  def ::(hd: Indicator) = Ambiguity(points.map { hd :: _ })
}


case class Path(steps: List[Offset])

sealed abstract class Resolution
case class Resolved(path: Path)
    extends Resolution
case class Unknown(ind: Indicator, location: Address, remaining: Address)
    extends Resolution
case class Ambiguous(name: Name, location: Address, issue: Ambiguity)
    extends Resolution



// in order to have a stable structure, unbounded repetition must engender an extra record level of uniformly-structured sub-records

object Structure {

  private sealed abstract class Traversal {
    def toResolution: Resolution
    def ::(next: (Offset, Indicator)): Traversal
  }

  private case class Traversed(offs: List[Offset])
      extends Traversal {
    def toResolution: Resolution = Resolved(Path(offs))
    def ::(next: (Offset,Indicator)): Traversal = copy(offs = next._1 :: offs)
  }

  private case class Not_Found(
      ind: Indicator,
      steps: List[Indicator],
      remaining: Address
    ) extends Traversal {
    def toResolution: Resolution = Unknown(ind, Address(steps), remaining)
    def ::(next: (Offset,Indicator)): Traversal = copy(steps = next._2 :: steps)
  }

  private case class Not_Unique(
      name: Name,
      steps: List[Indicator],
      issue: Ambiguity
    ) extends Traversal {
    def toResolution: Resolution = Ambiguous(name, Address(steps), issue)
    def ::(next: (Offset,Indicator)): Traversal = copy(steps = next._2 :: steps)
  }
}

sealed abstract class Structure {

  final def resolve(addr: Address): Resolution =
    traverse(addr.chain).toResolution

  protected def child_at(offset: Offset): Option[Structure]

  protected[shred] val mappings: Map[String, Ref]

  private def traverse(chain: List[Indicator]): Structure.Traversal = {
    import Structure._
    chain match {
      case Nil => Traversed(Nil)
      case indicator :: rest => indicator match {
        case offset: Offset =>
          child_at(offset) match {
            case None => Not_Found(indicator, Nil, Address(rest))
            case Some(child) => (offset, indicator) :: child.traverse(rest)
          }
        case name @ Name(ident) => mappings.get(ident) match {
          case None => Not_Found(indicator, Nil, Address(rest))
          //!!!!fix so we don't loose indicator here!!!
          case Some(Alias(addr)) => traverse(addr.chain ::: rest)
          case Some(ambiguity: Ambiguity) => Not_Unique(name, Nil, ambiguity)
        }
      }
    }
  }
}

//!!!!!semantics: local_mappings must not have a numeric key, since that would be indistinguishable from an Offset!!!!
final class Repeating_Structure(
    child_format: Structure,
    local_mappings: Seq[(String, Alias)]
  ) extends Structure {

  // ignore `offset`, since children are uniformly structured
  def child_at(offset: Offset): Option[Structure] = Some(child_format)

  protected[shred] val mappings: Map[String, Ref] = {
    val self_indicator = Forward_Offset(0)
    val self_child_mappings =
      child_format.mappings.map { case (name, ref) =>
        (name, self_indicator :: ref)
      }
    (self_child_mappings /: local_mappings) { case (mappings, (name, alias)) =>
      mappings + ((name, alias)) // local mappings hide child ones
    }
  }
}

object Fixed_Structure {

  private def merge(x: Map[String,Ref], y: Map[String,Ref]): Map[String, Ref] =
    (x /: y.iterator) { case (mappings, (name, ref)) =>
      mappings + 
          ((name,
            if (!mappings.contains(name)) ref
            else formulate_ambiguity(mappings(name), ref)))
    }

  private[this] def formulate_ambiguity(x_ref: Ref, y_ref: Ref): Ambiguity = {
    (x_ref, y_ref) match {
      case (Alias(x_addr),          Alias(y_addr)) =>
        Ambiguity(Seq(Address_Point(x_addr), Address_Point(y_addr)))
      case (Ambiguity(ambig_pts),   Alias(y_addr)) =>
        Ambiguity(ambig_pts :+ Address_Point(y_addr))
      case (Alias(x_addr),          Ambiguity(ambig_pts)) =>
        Ambiguity(Address_Point(x_addr) +: ambig_pts)
      case (Ambiguity(x_ambig_pts), Ambiguity(y_ambig_pts)) =>
        Ambiguity(x_ambig_pts ++ y_ambig_pts)
    }
  }
}

final class Fixed_Structure(
    children: Seq[Structure],
    local_mappings: Seq[(String, Alias)]
  ) extends Structure {

  def child_at(offset: Offset): Option[Structure] = offset.into(children)

  protected[shred] val mappings: Map[String, Ref] = {
    // prefer Name to Offset when (immediate) child given alias locally
    val calc_indicator: Int => Indicator = {
      val nChildren = children.size
      val child_names: Map[Int, Name] = local_mappings.collect {
        // when several names map to equiv. offset (either forward or reverse),
        // use name in last mapping
        case (name, Alias(Address(Forward_Offset(i)::Nil))) =>
          (i,             Name(name))
        case (name, Alias(Address(Reverse_Offset(i)::Nil))) if i <= nChildren =>
          (nChildren - i, Name(name))
      }.toMap
      child_offset =>
        child_names.getOrElse(child_offset, Forward_Offset(child_offset))
    }

    val child_mappings =
      children.zipWithIndex.map { case (child, i) =>
        val child_indicator = calc_indicator(i)
        child.mappings.map { case (name, ref) =>
          (name, child_indicator :: ref)
        }
      }.foldLeft(Map.empty[String, Ref]) {
        Fixed_Structure.merge _
      }
    (child_mappings /: local_mappings){ case (mappings, (name, alias)) =>
      mappings + ((name, alias)) // local mappings hide child ones
    }
  }
}


class Record(structure: Structure, children: Seq[Record]) {

  def get_child(path: Path): Option[Record] =
    child_at(path.steps)

  private def child_at(offsets: List[Offset]): Option[Record] =
    offsets match {
      case offset :: Nil  => offset.into(children)
      case offset :: rest => offset.into(children).flatMap { child =>
        child.child_at(rest)
      }
      case Nil => None
    }
}
