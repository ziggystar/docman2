package rxutils

import rx._
import java.util.prefs.Preferences
import java.util.Locale
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

  def createPreferenceVar[T](prefs: Preferences, name: String, default: T)(implicit store: PreferenceStorable[T]): Var[T] = {
    new rx.Var[T](store.get(prefs,name,default), s"pref:$name"){
      val saveHook = Obs(this)(store.put(prefs,name,this()))
    }
  }
}
