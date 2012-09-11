package rapture.html

import rapture.io._
import scala.collection.mutable.ListBuffer

object Forms {

  trait Context[In, Out] {
    def action(formField: Form#FormField[In, Widget]): Out
  }

  trait Widget
  class Dropdown extends Widget
  class RadioList extends Widget
  class Checkbox extends Widget
  class StringInput extends Widget
  class Textbox extends Widget
  class HtmlEditor extends Widget

  trait Parser[Value] {
    def parse(value: Option[String]): Value
    def serialize(value: Value): Option[String]
    def submitted(value: Option[String]): Boolean = value.isDefined
  }

  implicit def context[In]: Context[In, In] =
    new Context[In, In] {
      def action(formField: Form#FormField[In, Widget]): In = formField.value.get
    }

  case class Method(name: String)
  val Get = Method("get")
  val Post = Method("post")

  implicit val StringParser = new Parser[String] {
    def parse(s: Option[String]) = s.getOrElse("")
    def serialize(s: String): Option[String] = Some(s)
  }
  
  implicit val IntParser = new Parser[Int] {
    def parse(s: Option[String]): Int = s.get.toInt
    def serialize(value: Int) = Some(value.toString)
  }

  implicit val BooleanParser = new Parser[Boolean] {
    def parse(s: Option[String]) = s.isDefined
    def serialize(value: Boolean) = if(value) Some("") else None
    override def submitted(value: Option[String]): Boolean = true
  }

  abstract class Form(name: Symbol, params: Map[String, String] = Map(), val method: Method = Post,
      val action: Path = ^) {

    protected val formFields: ListBuffer[FormField[_, Widget]] = new ListBuffer[FormField[_, Widget]]
    val formName = name.name
    def view: Html5.Element[Html5.Flow]
  
    def submitted = formFields.forall(_.submitted)

    def save() = formFields.foreach(_.save())

    def formField[Value, WidgetType <: Widget](name: Symbol, label: String, cell: Cell[Value] = nullCell[Value])(implicit parser: Parser[Value], renderer: Renderer[Value, WidgetType]): FormField[Value, WidgetType] =
      new FormField[Value, WidgetType](label, name, cell)
      
    trait Renderer[Value, WidgetType <: Widget] {
      def render(field: FormField[Value, WidgetType]): Html5.Element[Html5.Flow]
    }

    implicit def stringRenderer[T] = new Renderer[T, StringInput] {
      def render(f: FormField[T, StringInput]): Html5.Element[Html5.Flow] =
        Html5.input(f.value map { v => Html5.value -> (if(v == null) "null" else v.toString) }, Html5.name -> f.name)
        
    }
    
    implicit val TextboxRenderer = new Renderer[String, Textbox] {
      def render(f: FormField[String, Textbox]): Html5.Element[Html5.Flow] = {
        import Html5._
        textarea(Html5.name -> f.name)(f.value.getOrElse(""): String)
      }
        
    }
    
    implicit val CheckboxRenderer = new Renderer[Boolean, Checkbox] {
      def render(f: FormField[Boolean, Checkbox]): Html5.Element[Html5.Flow] = {
        import Html5._
        span(
          input(`type` -> checkbox, Html5.name -> f.name),
          " ",
          f.label
        )

      }
        
    }

    private def nullCell[T] = new Cell[T] {
      def apply(): T = null.asInstanceOf[T]
      def update(t: T) = ()
    }

    class FormField[Value, +WidgetType <: Widget](val label: String, val name: Symbol, val cell: Cell[Value] = nullCell[Value])(implicit parser: Parser[Value], renderer: Renderer[Value, WidgetType]) {
      val fieldName: String = name.name
      def paramValue: Option[String] = params.get(formName+"_"+fieldName)
      def stringValue: Option[String] = paramValue orElse parser.serialize(cell())
      def submitted: Boolean = parser.submitted(paramValue)
      def value: Option[Value] = if(parser.submitted(stringValue)) Some(parser.parse(stringValue)) else None
      def render: Html5.Element[Html5.Flow] = renderer.render(this)
      def apply[Out]()(implicit n: Context[Value, Out]): Out =
        n.action(this)

      def save() = value foreach cell.update

      formFields += this
    }
  }

  trait FormLayout extends Form {
    implicit def renderContext[In]: Context[In, Html5.Element[Html5.Flow]] =
      new Context[In, Html5.Element[Html5.Flow]] {
        def action(formField: Form#FormField[In, Widget]): Html5.Element[Html5.Flow] = formField.render
      }
    
    def view: Html5.Element[Html5.Flow]
  }

  trait TabularLayout extends FormLayout {

    override def view = Html5.form(Html5.action -> action, Html5.method -> method.name)(
      Html5.table(
        Html5.tbody(
          formFields map { f =>
            Html5.tr(
              Html5.td(f.label),
              Html5.td(f.render)
            )
          }
        )
      )
    )
  }
}
