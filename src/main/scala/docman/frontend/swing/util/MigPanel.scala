package docman.frontend.swing.util

/* Copyright 2010 Trond Bjerkestrand
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

import javax.swing.JPanel
import net.miginfocom.swing.MigLayout

import scala.swing.{Component, Panel}


class MigPanel(
                val layoutConstraints: String = "",
                val columnConstraints: String = "",
                val rowConstraints: String = "") extends Panel {
  override lazy val peer: JPanel with SuperMixin = {
    val mig = new MigLayout(
      layoutConstraints,
      columnConstraints,
      rowConstraints
    )
    new javax.swing.JPanel(mig) with SuperMixin
  }

  //noinspection ScalaUnusedSymbol
  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]

  def add(comp: Component, constr: String = ""): Unit = peer.add(comp.peer, constr)
}