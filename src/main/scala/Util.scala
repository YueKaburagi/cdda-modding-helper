
package cddamod

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import scalaz.Scalaz._

import org.json4s._
import org.json4s.JsonDSL._


trait UT {
  def first[A,B](t: (A,B)): A = t._1
  def second[A,B](t: (A,B)): B = t._2

  def fFirst[A,B,C](f: A => C)(t: (A,B)): (C,B) = t match {
    case (a,b) => (f (a), b)
  }
  def fSecond[A,B,C](f: B => C)(t: (A,B)): (A,C) = t match {
    case (a,b) => (a, f (b))
  }
  def fSecondM[A,B,C,F[_]: Functor](f: B => F[C])(t: (A,B)): F[(A,C)] = t match {
    case (a,b) => f(b) map {(a,_)}
  }
  def fSecondE[A,B,C,E](f: B => E \/ C)(t: (A,B)): (E \/ (A,C)) = t match {
    case (a,b) => f(b) map {(a,_)}
  }
  def mapE[A,B,E](ts: List[A])(f: A => E \/ B): (E \/ List[B]) = 
    ts match {
      case Nil => List.empty.right
      case x :: xs => f(x) match {
	case l@ -\/(_) => l
	case \/-(r) => mapE(xs)(f) map {r :: _}
      }
    } // 千切れる

  def id[A](a: A): A = a

  def optToE[L,R](orElse: L)(o: Option[R]): \/[L,R] =
    o match {
      case Some(r) => r.right
      case None => orElse.left
    }

  private[this] def itself = this
  implicit def toUTListOps[A](ls: List[A]): UTListOps[A] = new UTListOps[A] {
    def tool = itself
    def self = ls
  } 
}

trait UTListOps[A] {
  protected[this] def tool: UT
  protected[this] def self: List[A]
  
  def mapE[B,E](f: A => E \/ B): (E \/ List[B]) =
    tool.mapE(self)(f)
}

trait DoAny extends UT {

  def lookup(jv: JValue, key: String): Option[JValue] = 
    jv findField {case (k,_) => k == key} match {
      case None => None
      case Some((k,v)) => v.some
    }

  def lookupE(jv: JValue)(jkey: JValue): (Error \/ JField) =
    jkey match {
      case JString(key) => lookupE(jv, key)
      case _ => UnmatchedValueType("\"\"", jkey).left
    }
  def lookupE(jv: JValue, key: String): (Error \/ JField) =
    jv match {
      case JObject(fs) => optToE(KeyNotFound(key, jv)){fs find {case (k,_) => k == key}}
      case _ => UnmatchedValueType("{\""+key+"\": ?}", jv).left
    }

  private[this] def itself = this
  implicit def toJVOps(jv: JValue): JVOps =
    new JVOps {
      def tool = itself
      def self = jv
    }

}
object JValueOrder {
  private[this] def rider[A](asc: Boolean)(ord: Order[A])(x: A)(y: A): Ordering =
    asc match {
      case true => ord order (x,y)
      case false => ord order (y,x)
    }
  def order(asc: Boolean)(x: JValue, y: JValue): Ordering = {
    (x,y) match {
      case (JString(a), JString(b)) =>
        rider(asc)(ToScalazOrderFromOrdering(scala.math.Ordering.String))(a)(b)
      case (JInt(a), JInt(b)) =>
        rider(asc)(ToScalazOrderFromOrdering(scala.math.Ordering.BigInt))(a)(b)
      case (JString(_), JInt(_)) => Ordering.LT
      case (JInt(_), JString(_)) => Ordering.GT
      case _ => Ordering.GT // 比較できないものは後ろへ
    }
  }
}

trait JVOps extends UT {
  protected[this] def tool: DoAny
  protected[this] def self: JValue
  def has(key: String): Boolean = this.lookup(key).isDefined
  def lookup(key: String): Option[JValue] =
    tool.lookup(self, key)
  def lookupE(key: String): (Error \/ JValue) =
    tool lookupE(self, key) map second
}

