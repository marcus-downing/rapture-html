package rapture.html

import scala.collection.mutable.ListMap

trait Widgets {
  trait Widget[+T]
  case class Checkbox[+T]() extends Widget[T]
  case class Dropdown[+T](options: List[(String, String)]) extends Widget[T]
  case class RadioList[T](options: List[T])(val id: T => String = ((t: Any) => t.toString),
      val description: T => String = ((t: Any) => t.toString)) extends Widget[T]
  case class TextArea[+T](width: Int = 8, height: Int = 80) extends Widget[T]
  case class HtmlEditor[+T]() extends Widget[T]
  case class StringInput[+T]() extends Widget[T]
  case class FileUploader[+T]() extends Widget[T]
}

