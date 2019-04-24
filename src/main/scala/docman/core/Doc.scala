package docman.core

import java.io.File

import org.apache.pdfbox.pdmodel.{PDDocument, PDDocumentInformation}
import resource._

/**
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
    (for(pd <- managed(PDDocument.load(f))) yield pd.getDocumentInformation).opt.get

  def sideCarFor(f: File): File = new File(f.getAbsolutePath.dropRight(3) + "smd")
  def fromFile(pdf: File): Doc = {
    val sidecar = sideCarFor(pdf)
    val pdfMeta = getMetaData(pdf)
    val properties = DProp.readPropertiesFromPDF(pdfMeta) ++
      Some(sidecar).filter(_.exists).map(DProp.readPropertiesFromFile).getOrElse(PropertyMap.empty)
    Doc(pdf,properties)
  }
}
