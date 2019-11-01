package docman.core

import java.io.File

import cats.effect.{IO, Resource}
import org.apache.pdfbox.pdmodel.{PDDocument, PDDocumentInformation}

/**
  * Old version of document based on a PropertyMap.
 * @param pdfFile
 * @param properties
 */
case class Doc(pdfFile: File, properties: PropertyMap){
  def sidecar: File = Doc.sideCarFor(pdfFile)
}

trait Persistable[T] {
  def persist(t: T): Unit
}

object DirectSidecarPersistable {
  implicit def docPersistable: Persistable[Doc] = doc => DProp.writePropertiesToFile(doc.properties, doc.sidecar)
}

object Doc{
  def getMetaData(f: File): PDDocumentInformation =
    Resource.fromAutoCloseable(IO(PDDocument.load(f))).use(pd => IO(pd.getDocumentInformation)).unsafeRunSync()

  def sideCarFor(f: File): File = new File(f.getAbsolutePath.dropRight(3) + "smd")
  def fromFile(pdf: File): Doc = {
    val sidecar = sideCarFor(pdf)
    val pdfMeta = getMetaData(pdf)
    val properties = DProp.readPropertiesFromPDF(pdfMeta) ++
      Some(sidecar).filter(_.exists).map(DProp.readPropertiesFromFile).getOrElse(PropertyMap.empty)
    Doc(pdf,properties)
  }
}
