package docman.config

import java.io.File
import java.nio.charset.Charset
import java.util.prefs.Preferences

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}


import scala.util.Try

case class SearchDir(dir: File, recursive: Boolean)
object SearchDir{
  implicit val fileReader: ReadWriter[File] = readwriter[String].bimap[File](_.getAbsolutePath,new File(_))
  implicit def rw: RW[SearchDir] = macroRW
}
case class Config(searchDirs: Seq[SearchDir] = Seq()){
  def asJson: String = write[Config](this)
}
object Config{
  implicit def rw: RW[Config] = macroRW

  def fromJson(js: String): Either[Throwable, Config] = Try(read[Config](js)).toEither

  private val key = "jsonConfig"
  private val charset: Charset = Charset.forName("UTF-8")

  def load(prefs: Preferences): Either[Throwable, Config] = (for {
    bytes <- Try(prefs.getByteArray(key,Config().asJson.getBytes(charset)))
    string = new String(bytes, charset)
    cfg <- Try(read[Config](string))
  } yield cfg).toEither

  def store(prefs: Preferences, cfg: Config): Unit = {
    prefs.putByteArray(key,write(cfg).getBytes(charset))
  }
}
