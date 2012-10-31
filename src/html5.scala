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

// Implementation of draft spec from about 2009. Not up to date.
class Html5 extends ElementClasses with HtmlDefs with AttributeClasses with Serialization {

  val id = new AttributeKey[Symbol, GlobalAttributes]("id")
  
  val lang = new AttributeKey[String, GlobalAttributes]("lang")
  
  val translate = new AttributeKey[String, AttributeType]("translate")
  
  val `class` = new AttributeKey[String, GlobalAttributes]("class")
  
  val cls = `class`
  
  val html = new Tag[Top, Nothing, HtmlAttributes]("html")
  
  val manifest = new AttributeKey[String, HtmlAttributes]("manifest")
  
  val head = new Tag[Metadata, Top, AttributeType]("head")
  
  val title = new Tag[Text, Metadata, GlobalAttributes]("title", block = false) with
      BaseAttributeKey[String, GlobalAttributes] { val key = "title" }
  
  val base = new Tag[Nothing, Metadata, BaseAttributes]("base")
  
  val href = new AttributeKey[Link, BaseAttributes with LinkAttributes with AAttributes with
      AreaAttributes]("href")
  
  val target = new AttributeKey[String, BaseAttributes with LinkAttributes with AAttributes with
      AreaAttributes with FormAttributes with InputAttributes with ButtonAttributes]("target")
  
  val link = new Tag[Nothing, Metadata, LinkAttributes]("link")
  
  val rel = new AttributeKey[String, LinkAttributes with AAttributes with AreaAttributes]("rel")
  
  val media = new AttributeKey[String, LinkAttributes with StyleAttributes with AAttributes with
      SourceAttributes with AreaAttributes]("media")
  
  val hreflang = new AttributeKey[String, LinkAttributes with AAttributes with AreaAttributes](
      "hreflang")
  
  val `type` = new AttributeKey[TypeOption, LinkAttributes with StyleAttributes with
      ScriptAttributes with AAttributes with EmbedAttributes with ObjectAttributes with
      SourceAttributes with AreaAttributes with InputAttributes with ButtonAttributes with
      CommandAttributes with BbAttributes with MenuAttributes]("type")
 
  val scriptType = new AttributeKey[MimeTypes.MimeType, ScriptAttributes]("type")

  val typ = `type`
  
  val sizes = new AttributeKey[String, LinkAttributes]("sizes")
  
  val meta = new Tag[Metadata, Metadata, MetaAttributes]("meta")
  
  val name = new AttributeKey[Symbol, MetaAttributes with IframeAttributes with ObjectAttributes
      with ParamAttributes with MapAttributes with FormAttributes with FieldsetAttributes with
      InputAttributes with ButtonAttributes with SelectAttributes with TextareaAttributes with
      OutputAttributes]("name")
  
  val httpEquiv = new AttributeKey[String, MetaAttributes]("http-equiv")
  
  val content = new AttributeKey[String, MetaAttributes]("content")
  
  val charset = new AttributeKey[String, MetaAttributes with ScriptAttributes]("charset")
  
  val style = new Tag[Text, Metadata with Flow, GlobalAttributes]("style") with
      BaseAttributeKey[String, GlobalAttributes] { val key = "style" }
  
  val scoped = new AttributeKey[String, StyleAttributes]("scoped")
  
  val script = new Tag[Text, Metadata with Phrasing, ScriptAttributes]("script", hardClose = true)
  
  val src = new AttributeKey[Link, ScriptAttributes with ImgAttributes with IframeAttributes with
      EmbedAttributes with VideoAttributes with AudioAttributes with SourceAttributes with
      InputAttributes]("src")
  
  val async = new AttributeKey[String, ScriptAttributes]("async") -> "async"
  
  val defer = new AttributeKey[String, ScriptAttributes]("defer") -> "defer"
  
  val noscript = new Tag[Text, Metadata with Phrasing, AttributeType]("noscript")
  
  val body = new Tag[Flow, Top, BodyAttributes]("body")
  
  val onbeforeunload = new AttributeKey[String, BodyAttributes]("onbeforeunload")
  
  val onerror = new AttributeKey[String, BodyAttributes]("onerror")
  
  val onhashchange = new AttributeKey[String, BodyAttributes]("onhashchange")
  
  val onload = new AttributeKey[String, BodyAttributes]("onload")
  
  val onmessage = new AttributeKey[String, BodyAttributes]("onmessage")
  
  val onoffline = new AttributeKey[String, BodyAttributes]("onoffline")
  
  val ononline = new AttributeKey[String, BodyAttributes]("ononline")
  
  val onpopstate = new AttributeKey[String, BodyAttributes]("onpopstate")
  
  val onresize = new AttributeKey[String, BodyAttributes]("onresize")
  
  val onstorage = new AttributeKey[String, BodyAttributes]("onstorage")
  
  val onunload = new AttributeKey[String, BodyAttributes]("onunload")
  
  val section = new Tag[Flow, Sectioning, AttributeType]("section")
  
  val nav = new Tag[Flow, Sectioning, AttributeType]("nav")
  
  val article = new Tag[Flow, Sectioning, AttributeType]("article")
  
  val aside = new Tag[Flow, Sectioning, AttributeType]("aside")
  
  val h1 = new Tag[Phrasing, Heading, AttributeType]("h1")
  
  val h2 = new Tag[Phrasing, Heading, AttributeType]("h2")
  
  val h3 = new Tag[Phrasing, Heading, AttributeType]("h3")
  
  val h4 = new Tag[Phrasing, Heading, AttributeType]("h4")
  
  val h5 = new Tag[Phrasing, Heading, AttributeType]("h5")
  
  val h6 = new Tag[Phrasing, Heading, AttributeType]("h6")
  
  val header = new Tag[Flow, Heading, AttributeType]("header")
  
  val footer = new Tag[Flow, Flow, AttributeType]("footer")
  
  val address = new Tag[Flow, Flow, AttributeType]("address")
  
  val p = new Tag[Phrasing, Flow, AttributeType]("p")
  
  val hr = new Tag[Nothing, Flow, AttributeType]("hr")
  
  val br = new Tag[Nothing, Phrasing, AttributeType]("br")
  
  val pre = new Tag[Phrasing, Flow, AttributeType]("pre")
  
  val dialog = new Tag[Definitions, Flow, AttributeType]("dialog")
  
  val blockquote = new Tag[Flow, Sectioning, BlockquoteAttributes]("blockquote")
  
  val ol = new Tag[ListItems, Flow, OlAttributes]("ol")
  
  val reversed = new AttributeKey[String, OlAttributes]("reversed")
  
  val start = new AttributeKey[Int, OlAttributes]("start")
  
  val ul = new Tag[ListItems, Flow, AttributeType]("ul")
  
  val li = new Tag[Flow, ListItems, LiAttributes]("li")
  
  val value = new AttributeKey[String, LiAttributes with ProgressAttributes with MeterAttributes
      with ParamAttributes with InputAttributes with ButtonAttributes with OptionAttributes](
      "value")
  
  val dl = new Tag[Definitions, Flow, AttributeType]("dl")
  
  val dt = new Tag[Phrasing, Definitions, AttributeType]("dt")
  
  val dd = new Tag[Flow, Definitions, AttributeType]("dd")
  
  def a[T <: ElementType] = new TransparentTag[T, AAttributes]("a", block = false)
  
  val ping = new AttributeKey[String, AAttributes with AreaAttributes]("ping")
  
  val q = new Tag[Phrasing, Phrasing, QAttributes]("q")
  
  val cite = new Tag[Phrasing, Phrasing, AttributeType]("cite") with BaseAttributeKey[String,
      BlockquoteAttributes with QAttributes with EditAttributes] { val key = "cite" }
  
  val em = new Tag[Phrasing, Phrasing, AttributeType]("em", block = false)
  
  val strong = new Tag[Phrasing, Phrasing, AttributeType]("strong", block = false)
  
  val small = new Tag[Phrasing, Phrasing, AttributeType]("small", block = false)
  
  val mark = new Tag[Phrasing, Phrasing, AttributeType]("mark")
  
  val dfn = new Tag[Phrasing, Phrasing, AttributeType]("dfn")
  
  val abbr = new Tag[Phrasing, Phrasing, AttributeType]("abbr", block = false)
  
  val time = new Tag[Phrasing, Phrasing, TimeAttributes]("time") with TypeOption { def typeName =
      "time" }
  
  val datetime = new AttributeKey[String, TimeAttributes with EditAttributes]("datetime")
  
  val progress = new Tag[Phrasing, Phrasing, ProgressAttributes]("progress")
  
  val max = new AttributeKey[Double, ProgressAttributes with MeterAttributes with InputAttributes](
      "max")
  
  val meter = new Tag[Phrasing, Phrasing, MeterAttributes]("meter")
  
  val min = new AttributeKey[Double, MeterAttributes with InputAttributes]("min")
  
  val low = new AttributeKey[Double, MeterAttributes]("low")
  
  val high = new AttributeKey[Double, MeterAttributes]("high")
  
  val optimum = new AttributeKey[Double, MeterAttributes]("optimum")
  
  val code = new Tag[Phrasing, Phrasing, AttributeType]("code")
  
  val `var` = new Tag[Phrasing, Phrasing, AttributeType]("var")
  
  val samp = new Tag[Phrasing, Phrasing, AttributeType]("samp")
  
  val kbd = new Tag[Phrasing, Phrasing, AttributeType]("kbd")
  
  val sup = new Tag[Phrasing, Phrasing, AttributeType]("sup", block = false)
  
  val sub = new Tag[Phrasing, Phrasing, AttributeType]("sub", block = false)
  
  val span = new Tag[Phrasing, Phrasing, AttributeType]("span", block = false) with BaseAttributeKey[Int,
      ColAttributes] { val key = "span" }
  
  val i = new Tag[Phrasing, Phrasing, AttributeType]("i", block = false)
  
  val b = new Tag[Phrasing, Phrasing, AttributeType]("b", block = false)
  
  val bdo = new Tag[Phrasing, Phrasing, BdoAttributes]("bdo")
  
  val dir = new AttributeKey[String, GlobalAttributes]("dir")
  
  val ruby = new Tag[Phrasing, Phrasing, AttributeType]("ruby")
  
  val rt = new Tag[Nothing, Phrasing, AttributeType]("rt")
  
  val rp = new Tag[Nothing, Phrasing, AttributeType]("rp")
  
  def ins[T <: ElementType] = new TransparentTag[T, EditAttributes]("ins")
  
  def del[T <: ElementType] = new TransparentTag[T, EditAttributes]("del")
  
  val figure = new Tag[Flow, Sectioning, AttributeType]("figure")
  
  val img = new Tag[Nothing, Embedded, ImgAttributes]("img")
  
  val alt = new AttributeKey[String, ImgAttributes with AreaAttributes with InputAttributes]("alt")
  
  val usemap = new AttributeKey[String, ImgAttributes with ObjectAttributes]("usemap")
  
  val ismap = new AttributeKey[String, ImgAttributes]("ismap")
  
  val width = new AttributeKey[Int, ImgAttributes with IframeAttributes with EmbedAttributes with
      ObjectAttributes with VideoAttributes with CanvasAttributes with InputAttributes]("width")
  
  val height = new AttributeKey[Int, ImgAttributes with IframeAttributes with EmbedAttributes with
      ObjectAttributes with VideoAttributes with CanvasAttributes with InputAttributes]("height")
  
  val iframe = new Tag[Text, Embedded, IframeAttributes]("iframe")
  
  val embed = new Tag[Text, Embedded, EmbedAttributes]("embed")
  
  val sandbox = new AttributeKey[String, IframeAttributes]("sandbox")
  
  val seamless = new AttributeKey[String, IframeAttributes]("seamless")
  
  def `object`[T <: Embedded] = new TransparentTag[T, ObjectAttributes]("object")
  
  val data = new AttributeKey[String, ObjectAttributes]("data")
  
  // Positioning semantics are more complicated...
  val param = new Tag[Flow, Embedded, ParamAttributes]("param")
  
  // Positioning semantics are more complicated...
  def video[T <: Embedded with Interactive] = new TransparentTag[T, VideoAttributes]("video")
  
  val poster = new AttributeKey[String, VideoAttributes]("poster")
  
  val autobuffer = new AttributeKey[String, VideoAttributes with AudioAttributes]("autobuffer")
  
  val autoplay = new AttributeKey[String, VideoAttributes with AudioAttributes]("autoplay")
  
  val loop = new AttributeKey[String, VideoAttributes with AudioAttributes]("loop")
  
  val controls = new AttributeKey[String, VideoAttributes with AudioAttributes]("controls")
  
  def audio[T <: Embedded with Interactive] = new TransparentTag[T, AudioAttributes]("audio")
  
  val source = new Tag[Nothing, Flow, SourceAttributes]("source")
  
  def canvas[T <: Embedded] = new TransparentTag[T, CanvasAttributes]("canvas")
  
  val map = new Tag[Flow, Flow, MapAttributes]("map")
  
  val area = new Tag[Nothing, Phrasing, AreaAttributes]("area")
  
  val coords = new AttributeKey[String, AreaAttributes]("coords")
  
  val shape = new AttributeKey[String, AreaAttributes]("shape")
  
  val table = new Tag[TableItems, Flow, AreaAttributes]("table")
  
  val caption = new Tag[Phrasing, TableItems, AttributeType]("tr")
  
  val colgroup = new Tag[ColItems, TableItems, ColAttributes]("colgroup")
  
  val col = new Tag[Nothing, ColItems, ColAttributes]("col")
  
  val tbody = new Tag[TrItems, TableItems, AttributeType]("tbody")
  
  val thead = new Tag[TrItems, TableItems, AttributeType]("thead")
  
  val tfoot = new Tag[TrItems, TableItems, AttributeType]("tfoot")
  
  val tr = new Tag[TdItems, TrItems, AttributeType]("tr")
  
  val td = new Tag[Flow, TdItems, TdAttributes]("td")
  
  val colspan = new AttributeKey[Int, TdAttributes with ThAttributes]("colspan")
  
  val rowspan = new AttributeKey[Int, TdAttributes with ThAttributes]("rowspan")
  
  val headers = new AttributeKey[Int, TdAttributes with ThAttributes]("headers")
  
  val th = new Tag[Flow, TdItems, ThAttributes]("th")
  
  val scope = new AttributeKey[String, ThAttributes]("scope")
  
  val form = new Tag[Flow, Flow, FormAttributes]("form") with BaseAttributeKey[String,
      ObjectAttributes with FieldsetAttributes with LabelAttributes with InputAttributes with
      ButtonAttributes with SelectAttributes with TextareaAttributes with OutputAttributes] {
      val key = "form" }
  
  val acceptCharset = new AttributeKey[String, FormAttributes]("accept-charset")
  
  val action = new AttributeKey[Link, FormAttributes with InputAttributes with ButtonAttributes](
      "action")
  
  val autocomplete = new BooleanAttributeKey[FormAttributes with InputAttributes]("autocomplete") {
    override def trueValue = "on"
    override def falseValue = "off"
  }
  
  val enctype = new AttributeKey[MimeTypes.MimeType, FormAttributes with InputAttributes with
      ButtonAttributes]("enctype")
  
  val method = new AttributeKey[HttpMethods.FormMethod, FormAttributes with InputAttributes with ButtonAttributes](
      "method")
  
  val novalidate = new AttributeKey[Boolean, FormAttributes with InputAttributes with
      ButtonAttributes]("novalidate")
  
  val fieldset = new Tag[Flow, Flow, FieldsetAttributes]("fieldset")
  
  val disabled = new AttributeKey[Boolean, FieldsetAttributes with InputAttributes with
      ButtonAttributes with SelectAttributes with OptgroupAttributes with OptionAttributes with
      TextareaAttributes with CommandAttributes]("disabled")
  
  val label = new Tag[Phrasing, Phrasing with Interactive, LabelAttributes]("label") with
      BaseAttributeKey[String, OptionAttributes with CommandAttributes with MenuAttributes] {
      val key = "label" }
  
  val `for` = new AttributeKey[Symbol, FieldsetAttributes with OutputAttributes with
      LabelAttributes]("for")
 
  val forName = `for`

  val input = new Tag[Nothing, Phrasing with Interactive, InputAttributes]("input")
  
  val accept = new AttributeKey[String, InputAttributes]("accept")

  val autofocus = new AttributeKey[Boolean, InputAttributes with ButtonAttributes with
      SelectAttributes with TextareaAttributes]("autofocus")
  
  //val checked = new AttributeKey[Boolean, InputAttributes with CommandAttributes]("checked")
  val checked = new Attribute[InputAttributes]("checked", "checked")

  val list = new AttributeKey[String, InputAttributes]("list")
  
  val maxlength = new AttributeKey[Int, InputAttributes with TextareaAttributes]("maxlength")
  
  val multiple = new AttributeKey[Boolean, InputAttributes with SelectAttributes]("multiple")
  
  val pattern = new AttributeKey[String, InputAttributes]("pattern")
  
  val placeholder = new AttributeKey[String, InputAttributes]("placeholder")
  
  val readonly = new AttributeKey[Boolean, InputAttributes with TextareaAttributes]("readonly")
  
  val required = new AttributeKey[Boolean, InputAttributes with TextareaAttributes]("required")
  
  val size = new AttributeKey[Int, InputAttributes with SelectAttributes]("size")
  
  val step = new AttributeKey[String, InputAttributes]("step")
  
  val button = new Tag[Phrasing, Phrasing with Interactive, ButtonAttributes]("button") with
      TypeOption { def typeName = "button" }
  
  val select = new Tag[OptionItems, Phrasing with Interactive, SelectAttributes]("select")
  
  // Need to include options contents
  val datalist = new Tag[OptionItems, Phrasing, AttributeType]("datalist")

  val optgroup = new Tag[OptionItems, Phrasing, OptgroupAttributes]("optgroup")
  
  val option = new Tag[Text, OptionItems, OptionAttributes]("option")
  
  val selected = new AttributeKey[Boolean, OptionAttributes]("selected")
  
  val textarea = new Tag[Text, Phrasing with Interactive, TextareaAttributes]("textarea",
      block = false)
  
  val cols = new AttributeKey[Int, TextareaAttributes]("cols")
  
  val rows = new AttributeKey[Int, TextareaAttributes]("rows")
  
  val wrap = new AttributeKey[Boolean, TextareaAttributes]("wrap")
  
  val output = new Tag[Phrasing, Phrasing, OutputAttributes]("output")
  
  val details = new Tag[Flow, Interactive, DetailsAttributes]("details")
  
  val open = new AttributeKey[Boolean, DetailsAttributes]("open")
  
  val command = new Tag[Nothing, Metadata with Phrasing, CommandAttributes]("command")
  
  val icon = new AttributeKey[String, CommandAttributes]("icon")
  
  val radiogroup = new AttributeKey[String, CommandAttributes]("radiogroup")
  
  val default = new AttributeKey[String, CommandAttributes]("default")
  
  val bb = new Tag[Phrasing, Phrasing with Interactive, BbAttributes]("bb")
  
  val menu = new Tag[ListItems, Phrasing with Interactive, MenuAttributes]("menu")
  
  val legend = new Tag[Phrasing, ElementType, AttributeType]("legend")
  
  val div = new Tag[Flow, Flow, AttributeType]("div")
 
  trait TypeOption { def typeName: String; override def toString = typeName }
  val hidden = new TypeOption { def typeName = "hidden" }
  val text = new TypeOption { def typeName = "text" }
  val tel = new TypeOption { def typeName = "tel" }
  val url = new TypeOption { def typeName = "url" }
  val email = new TypeOption { def typeName = "email" }
  val password = new TypeOption { def typeName = "password" }
  val date = new TypeOption { def typeName = "date" }
  val month = new TypeOption { def typeName = "month" }
  val week = new TypeOption { def typeName = "week" }
  val datetimeLocal = new TypeOption { def typeName = "datetime-local" }
  val number = new TypeOption { def typeName = "number" }
  val range = new TypeOption { def typeName = "range" }
  val color = new TypeOption { def typeName = "color" }
  val checkbox = new TypeOption { def typeName = "checkbox" }
  val radio = new TypeOption { def typeName = "radio" }
  val file = new TypeOption { def typeName = "file" }
  val submit = new TypeOption { def typeName = "submit" }
  val image = new TypeOption { def typeName = "image" }
  val reset = new TypeOption { def typeName = "reset" }


  // Not sure if these should be here
  val onclick = new AttributeKey[String, GlobalAttributes]("onclick")
  val onchange = new AttributeKey[String, GlobalAttributes]("onchange")
  val onmouseover = new AttributeKey[String, GlobalAttributes]("onmouseover")
  val onmouseout = new AttributeKey[String, GlobalAttributes]("onmouseout")

}

object Html5 extends Html5
