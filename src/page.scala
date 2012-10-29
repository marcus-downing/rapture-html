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

import rapture.io._
import Base._

object Layout {

  import MimeTypes._

  trait PageMetadata { page: Page =>
    override def metas: List[Html5.Element[Html5.Metadata]] =
      (metaData.toList map { case (k, v) => Html5.meta(Html5.name -> k, Html5.content -> v)() }) :::
          page.metas
    
    def metaData: Map[Symbol, String] = Map(
      'description -> metaDescription,
      'keywords -> metaKeywords.mkString(","),
      'author -> metaAuthor
    )

    def metaDescription: String
    def metaKeywords: List[String]
    def metaAuthor: String
  }

  trait JQueryUi extends Page { this: JQuery =>
    def jQueryUiLocation = Http / "ajax.googleapis.com" / "ajax" / "libs" / "jqueryui" / "1.8.23" /
        "jquery-ui.min.js"
    
    override def scripts: List[Html5.Element[Html5.Metadata]] =
      Html5.script(Html5.scriptType -> `text/javascript`, Html5.src -> jQueryUiLocation) :: super.scripts
  }

  trait JQuery extends Page {

    def jQueryLocation: HttpUrl = Http / "ajax.googleapis.com" / "ajax" / "libs" / "jquery" / "1.7.2" / "jquery.min.js"

    override def scripts: List[Html5.Element[Html5.Metadata]] =
      Html5.script(Html5.scriptType -> `text/javascript`, Html5.src -> jQueryLocation) :: super.scripts
  }

  abstract class Page { page =>

    def httpStatus = 200

    def doctype = "<!DOCTYPE html>"

    def stylesheets: List[Stylesheet] = Nil
    case class Stylesheet(link: Link)

    def lang: String = "en"
    def title: String

    def links: List[Html5.Element[Html5.Metadata]] =
      stylesheets map { ss => Html5.link(Html5.rel -> "stylesheet", Html5.href -> ss.link)() }
    
    def scripts: List[Html5.Element[Html5.Metadata]] = Nil
    def styles: List[Html5.Element[Html5.Metadata]] = Nil
    def metas: List[Html5.Element[Html5.Metadata]] = Nil

    def head =
      Html5.title(page.title) :: styles.reverse ::: links.reverse ::: scripts.reverse ::: metas

    def body: List[Html5.Element[Html5.Flow]]

    def document =
      Html5.html(Html5.lang -> page.lang)(
        Html5.head(page.head: _*),
        Html5.body(page.body: _*)
      )

    def stream: Input[Char] = {
      val sb = new StringBuilder
      sb.append(doctype)
      sb.append(document.toString)
      sb.toString.input[Char]
    }
  }

  trait Bootstrap extends Page {
   
    def bootstrapLocation = Http / "twitter.github.com" / "bootstrap" / "1.4.0" / "bootstrap.min.css"

    override def links: List[Html5.Element[Html5.Metadata]] =
      Html5.link(Html5.rel -> "stylesheet", Html5.href -> bootstrapLocation)() :: super.links

  }

  import Forms._
  import Html5._
   
  trait TinyMce extends Page {

    def tinyMceLocation: Link

    override def scripts: List[Html5.Element[Html5.Metadata]] =
      Html5.script(Html5.scriptType -> `text/javascript`, Html5.src -> tinyMceLocation)() :: super.links
 
  }
  
  trait TinyMceForm { this: WebForm =>

    implicit val tinyMceEditorRenderer =
      new Renderer[String, Field[String], HtmlEditor] {
        def render(f: Field[String], w: HtmlEditor) =
          textarea(style -> "width: 100%", Html5.name -> f.name, cls -> "mceEditorCustom")(raw(f.fieldValue))
      }
  }
  
}
