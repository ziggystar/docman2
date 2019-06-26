package docman.utils

import java.util.Locale

import scala.language.implicitConversions

/**
 * @author Thomas Geier
 * @since 5/25/14
 */

trait Isomorphism[A,B]{
  def forward: A => B
  def backward: B => A
}

object Isomorphism{
  def apply[A,B](f: A => B, b: B => A): Isomorphism[A,B] = new Isomorphism[A,B] {
    override def forward: A => B = f

    override def backward: B => A = b
  }
  implicit def revIso[A,B](ab: Isomorphism[A,B]): Isomorphism[B,A] = new Isomorphism[B,A] {
    override def forward: (B) => A = ab.backward
    override def backward: (A) => B = ab.forward
  }

  implicit val LocaleAsLanguageCode: StringIso[Locale] = StringIso[Locale](_.toLanguageTag, Locale.forLanguageTag)
}

case class StringIso[A](encode: A => String, decode: String => A) extends Isomorphism[A,String]{
  override def forward: A => String = encode
  override def backward: String => A = decode
}