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
case class Doc(pdfFile: File, sidecar: File, properties: Map[DocProperty,String])

object Doc{
  def getMetaData(f: File): PDDocumentInformation =
    (for(pd <- managed(PDDocument.load(f))) yield pd.getDocumentInformation).opt.get
  def fromFile(pdf: File): Doc = {
    val sidecar = new File(pdf.getAbsolutePath.dropRight(3) + "smd")
    val pdfMeta = getMetaData(pdf)
    val mapFromPdf: Map[DocProperty, String] = DocProperty.ALL.foldLeft(Map[DocProperty,String]()){case (map, prop) =>
      map + (prop -> prop.pdfboxExtractor(pdfMeta))
    }
    val mapFromSidecar: Map[DocProperty, String] = Option(sidecar).filter(_.exists).map(sc =>
      ( for(src <- managed(Source.fromFile(sc))) yield {
        (for{
          line <- src.getLines()
          prop <- DocProperty.ALL
        } yield prop.deserialize(line).map(prop -> _)).flatten.toMap
      }).opt).flatten.getOrElse(Map())
    Doc(pdf,sidecar,mapFromPdf ++ mapFromSidecar)
  }

  /** @return The updated Doc object. */
  def saveMeta(d: Doc) {
    for(writer <- managed(new FileWriter(d.sidecar))) {
      d.properties.map{case (prop, value) => prop.serializeToLine(value)}.foreach{line =>
        writer.write(line + "\n")
      }
    }
  }
}
