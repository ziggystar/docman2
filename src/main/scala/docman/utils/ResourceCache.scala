package docman.utils

import scala.language.reflectiveCalls
import scala.util.Try

import scala.jdk.CollectionConverters._

/** Manages a pool of opened resources. Currently it's hard to prevent escaping of references. Maybe the return type
  * of `get` should be changed. Also async operations might be useful.
  * @author Thomas Geier
  */
class ResourceCache[K,V <: {def close(): Unit}](val open: K => V, val size: Int = 3) {
  private val cache = new java.util.concurrent.ConcurrentHashMap[K,Try[V]]
  private val lastAccessed =  new java.util.concurrent.ConcurrentHashMap[K,Long]
  @volatile private var lastTime = 0L

  def get[A](k: K)(f: V => A): Try[A] = this.synchronized{
    val existingKey: Option[K] = Some(k).filter(cache.containsKey)
    val v: Try[V] = existingKey.fold(create(k))(cache.get)
    lastAccessed.put(k, lastTime)
    lastTime = lastTime + 1
    v.map(f)
  }

  private def create(k: K): Try[V] = {
    if(cache.size == size){
      val toRemove: K = lastAccessed.entrySet.asScala.minBy(_.getValue).getKey
      cache.get(toRemove).foreach(_.close())
      cache.remove(toRemove)
      lastAccessed.remove(toRemove)
    }

    val newV = Try(open(k))
    cache.put(k, newV)
    newV
  }
}
