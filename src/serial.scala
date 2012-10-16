/**************************************************************************************************
Rapture HTML Library
Version 0.7.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2012 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/
package rapture.html

trait Serialization { this: Html5 =>

  def doSerialize(block: Boolean, name: String, attributes: Map[String, String],
      body: Seq[Element[_]], sb: StringBuilder, n: Int, indent: Boolean, hardClose: Boolean = true) =
    if(block) {
      if(!indent) sb.append("\n")
      sb.append("  "*n)
      sb.append("<")
      sb.append(name)
      for((k, v) <- attributes if v != null) {
        sb.append(" ")
        sb.append(k)
        if(v != "") {
          sb.append("=\"")
          sb.append(v)
          sb.append("\"")
        }
      }
      body.toList match {
        case Nil =>
          if(hardClose) {
            sb.append("></")
            sb.append(name)
            sb.append(">")
          } else sb.append("/>")
        case h :: t =>
          sb.append(">\n")
          h.serialize(sb, n + 1, true)
          t.foreach(_.serialize(sb, n + 1, false))
          sb.append("\n")
          sb.append("  "*n)
          sb.append("</")
          sb.append(name)
          sb.append(">")
      }
    } else {
      if(indent) sb.append("  "*n)
      sb.append("<")
      sb.append(name)
      for((k, v) <- attributes) {
        sb.append(" ")
        if(v != "") {
          sb.append(k)
          sb.append("=\"")
          sb.append(v)
          sb.append("\"")
        }
      }
      sb.append(">")
      body.foreach(_.serialize(sb, n, false))
      sb.append("</")
      sb.append(name)
      sb.append(">")
    }
}

