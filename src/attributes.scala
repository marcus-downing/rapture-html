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

trait AttributeClasses { this: Html5 =>

  trait HtmlAttributes extends AttributeType
  trait BaseAttributes extends AttributeType
  trait LinkAttributes extends AttributeType
  trait MetaAttributes extends AttributeType
  trait StyleAttributes extends AttributeType
  trait ScriptAttributes extends AttributeType
  trait BodyAttributes extends AttributeType
  trait BlockquoteAttributes extends AttributeType
  trait OlAttributes extends AttributeType
  trait LiAttributes extends AttributeType
  trait AAttributes extends AttributeType
  trait QAttributes extends AttributeType
  trait TimeAttributes extends AttributeType
  trait ProgressAttributes extends AttributeType
  trait MeterAttributes extends AttributeType
  trait BdoAttributes extends AttributeType
  trait EditAttributes extends AttributeType
  trait ImgAttributes extends AttributeType
  trait IframeAttributes extends AttributeType
  trait EmbedAttributes extends AttributeType
  trait ObjectAttributes extends AttributeType
  trait ParamAttributes extends AttributeType
  trait VideoAttributes extends AttributeType
  trait AudioAttributes extends AttributeType
  trait SourceAttributes extends AttributeType
  trait CanvasAttributes extends AttributeType
  trait MapAttributes extends AttributeType
  trait AreaAttributes extends AttributeType
  trait ColAttributes extends AttributeType
  trait TdAttributes extends AttributeType
  trait ThAttributes extends AttributeType
  trait FormAttributes extends AttributeType
  trait FieldsetAttributes extends AttributeType
  trait LabelAttributes extends AttributeType
  trait InputAttributes extends AttributeType
  trait ButtonAttributes extends AttributeType
  trait SelectAttributes extends AttributeType
  trait OptgroupAttributes extends AttributeType
  trait OptionAttributes extends AttributeType
  trait TextareaAttributes extends AttributeType
  trait OutputAttributes extends AttributeType
  trait DetailsAttributes extends AttributeType
  trait CommandAttributes extends AttributeType
  trait BbAttributes extends AttributeType
  trait MenuAttributes extends AttributeType

  trait GlobalAttributes extends HtmlAttributes with BaseAttributes with LinkAttributes with
      MetaAttributes with StyleAttributes with ScriptAttributes with BodyAttributes with
      BlockquoteAttributes with OlAttributes with LiAttributes with AAttributes with QAttributes
      with TimeAttributes with ProgressAttributes with MeterAttributes with BdoAttributes with
      EditAttributes with ImgAttributes with IframeAttributes with EmbedAttributes with
      ObjectAttributes with ParamAttributes with VideoAttributes with AudioAttributes with
      SourceAttributes with CanvasAttributes with MapAttributes with AreaAttributes with
      ColAttributes with TdAttributes with ThAttributes with FormAttributes with FieldsetAttributes
      with LabelAttributes with InputAttributes with ButtonAttributes with SelectAttributes with
      OptgroupAttributes with OptionAttributes with TextareaAttributes with OutputAttributes with
      DetailsAttributes with CommandAttributes with BbAttributes with MenuAttributes

}
