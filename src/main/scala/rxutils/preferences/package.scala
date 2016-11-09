package rxutils

import java.util.prefs.Preferences

import rx.lang.scala.Subject
import rx.lang.scala.subjects.BehaviorSubject
import utils.StringIso

/** Utilities for accessing preferences through rx variables.
 * @author Thomas Geier
 * @since 5/25/14
 */
package object preferences {
  trait PreferenceStorable[T]{
    def get(p: Preferences, name: String, default: T): T
    def put(p: Preferences, name: String, value: T): Unit
  }

  implicit def prefAsString[T: StringIso] = new PreferenceStorable[T]{
    val mapping = implicitly[StringIso[T]]
    override def get(p: Preferences, name: String, default: T): T = mapping.decode(p.get(name,mapping.encode(default)))
    override def put(p: Preferences, name: String, value: T): Unit = p.put(name,mapping.encode(value))
  }

  def createPreferenceVar[T](prefs: Preferences, name: String, default: T)(implicit store: PreferenceStorable[T]): Subject[T] = {
    val s = BehaviorSubject[T]()
    s.onNext(store.get(prefs,name,default))
    s.foreach{ t => store.put(prefs,name,t)}
    s
  }
}
