package docman

import java.io.{FileWriter, File}
import org.apache.pdfbox.pdmodel.{PDDocument, PDDocumentInformation}
import resource._
import scala.io.Source

/**
 * @param pdfFile
 * @param sidecar Sidecar file. Does not necessarily exist.
 * @param properties
 */
case class Doc(pdfFile: File, sidecar: File, properties: PropertyMap){
  def saveMeta(): Unit = {
    DProp.writePropertiesToFile(properties,sidecar)
  }
}

object Doc{
  def getMetaData(f: File): PDDocumentInformation =
    (for(pd <- managed(PDDocument.load(f))) yield pd.getDocumentInformation).opt.get

  def fromFile(pdf: File): Doc = {
    val sidecar = new File(pdf.getAbsolutePath.dropRight(3) + "smd")
    val pdfMeta = getMetaData(pdf)
    val properties = DProp.readPorpoertiesFromPDF(pdfMeta) ++
      Some(sidecar).filter(_.exists).map(DProp.readPropertiesFromFile).getOrElse(PropertyMap.empty)
    Doc(pdf,sidecar,properties)
  }
}
