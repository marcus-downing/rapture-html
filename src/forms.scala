package rapture.html

import rapture.io._
import scala.collection.mutable.ListBuffer

import rapture.orm.Time._

object Forms {

  implicit val formLogZone = Zone("form")

  trait Context[In, Out] {
    def action(formField: Form#FormField[In, Widget]): Out
  }

  trait Widget
  class Dropdown extends Widget
  class RadioList extends Widget
  class Checkbox extends Widget
  class StringInput extends Widget
  class TextArea extends Widget
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

  implicit val DateParser = new Parser[Date] {
    def parse(string: Option[String]) = string map { s =>
      val ps = s.split("\\/").map(_.toInt)
      Date(ps(0), ps(1), ps(2))
    } getOrElse Date.unapply(0L).get
    def serialize(d: Date) = Some(d.format(DateFormat("dd/MM/yy")))
  }

  implicit val DateTimeParser = new Parser[DateTime] {
    def parse(string: Option[String]) = string map { s =>
      val ps = s.split(" ")
      val ds = ps(0).split("\\/").map(_.toInt)
      val ts = ps(1).split(":").map(_.toInt)
      DateTime(Date(ds(0), ds(1), ds(2)), ts(0), ts(1), ts(2))
    } getOrElse DateTime.unapply(0L).get
    def serialize(d: DateTime) = Some(d.format(DateFormat("dd/MM/yy"), TimeFormat("HH:mm:ss")))
  }

  abstract class Form(name: Symbol, params: Map[String, String] = Map(), val method: Method = Post,
      val action: Path) {

    implicit def richFunction(fn: String => List[String]) = new {
      def &&(fn2: String => List[String]): String => List[String] =
        { s: String => fn(s) ::: fn2(s) }
    }

    protected val formFields: ListBuffer[FormField[_, Widget]] = new ListBuffer[FormField[_, Widget]]
    val formName = name.name
    def view: Html5.Element[Html5.Flow]
  
    def submitted = formFields.forall(_.submitted) && params.contains("_submit")

    def validated = formFields.forall(_.validated)

    def showValidation = submitted && !validated

    def complete = submitted && validated

    def save() = formFields.foreach(_.save())

    def formField[Value, WidgetType <: Widget](name: Symbol, label: String, cell: Cell[Value] = nullCell[Value], validate: String => List[String] = { s => Nil }, help: String = "")(implicit parser: Parser[Value], renderer: Renderer[Value, WidgetType]): FormField[Value, WidgetType] =
      new FormField[Value, WidgetType](label, name, cell, validate, help)
      
    trait Renderer[Value, WidgetType <: Widget] {
      def render(field: FormField[Value, WidgetType]): Html5.Element[Html5.Flow]
    }

    implicit def stringRenderer[T] = new Renderer[T, StringInput] {
      def render(f: FormField[T, StringInput]): Html5.Element[Html5.Flow] =
        Html5.input(f.value map { v => Html5.value -> (if(v == null) "null" else v.toString) }, Html5.name -> f.name)
        
    }
    
    implicit val TextAreaRenderer = new Renderer[String, TextArea] {
      def render(f: FormField[String, TextArea]): Html5.Element[Html5.Flow] = {
        import Html5._
        textarea(Html5.name -> f.name)(f.value.getOrElse(""): String)
      }
        
    }
    
    implicit val CheckboxRenderer = new Renderer[Boolean, Checkbox] {
      def render(f: FormField[Boolean, Checkbox]): Html5.Element[Html5.Flow] = {
        import Html5._
        span(
          input(`type` -> checkbox, Html5.name -> f.name, if(f.value.getOrElse(false)) Some(Html5.checked) else None),
          " "
        )
      }
    }

    implicit val DateTimeRenderer = new Renderer[DateTime, StringInput] {
      def render(f: FormField[DateTime, StringInput]): Html5.Element[Html5.Flow] = {
        import Html5._
        input(`type` -> text, placeholder -> "DD/MM/YY hh:mm:ss", Html5.name -> f.name, value -> f.stringValue.getOrElse(""))
      }
    }

    implicit val DateRenderer = new Renderer[Date, StringInput] {
      def render(f: FormField[Date, StringInput]): Html5.Element[Html5.Flow] = {
        import Html5._
        input(`type` -> text, placeholder -> "DD/MM/YY", Html5.name -> f.name, value -> f.stringValue.getOrElse(""))
      }
    }

    def radioListRenderer[T](opts: List[(Int, T)], getString: T => String)(implicit parser: Parser[T]) =
      new Renderer[T, RadioList] {
        def render(f: FormField[T, RadioList]): Html5.Element[Html5.Flow] = {
          import Html5._
          div(
            opts flatMap { opt => List[Element[Flow]](
              input(Html5.name -> f.name, value -> opt._1.toString, `type` -> radio, if(f.stringValue.getOrElse("") == opt._1.toString) Some(checked) else None),
              " "+getString(opt._2),
              br
            ) }
          )
        }
      }

    private def nullCell[T] = new Cell[T] {
      def apply(): T = null.asInstanceOf[T]
      def update(t: T) = ()
    }

    class FormField[Value, +WidgetType <: Widget](val label: String, val name: Symbol, val cell: Cell[Value] = nullCell[Value], val validate: String => List[String] = { s => Nil }, val help: String = "")(implicit parser: Parser[Value], renderer: Renderer[Value, WidgetType]) {
      val fieldName: String = name.name
      
      def validated: Boolean = validationIssues.isEmpty
      lazy val validationIssues = stringValue.map(validate).getOrElse(Nil)
      def paramValue: Option[String] = params.get(fieldName)
      def stringValue: Option[String] = paramValue orElse parser.serialize(cell())
      def submitted: Boolean = parser.submitted(paramValue)
      def value: Option[Value] = if(parser.submitted(stringValue)) Some(parser.parse(stringValue)) else None
      def render: Html5.Element[Html5.Flow] = renderer.render(this)
      def apply[Out]()(implicit n: Context[Value, Out]): Out =
        n.action(this)

      def save() = value foreach cell.update

      def validation: List[String] = if(showValidation) validationIssues else Nil

      formFields += this
    }

    val validPhoneNumber = { s: String => if(s.matches("""^[+\- ()0-9]*$""")) Nil else List("Please enter a valid telephone number") }
    val validEmailAddress = { s: String => if(s.matches("""^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$""")) Nil else List("Please enter a valid email address") }
    val optValidEmailAddress = { s: String => if(s.matches("""^([_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4}))?$""")) Nil else List("Please enter a valid email address") }
    val validDateTime = { s: String => if(s.matches("[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]")) Nil else List("Please enter a valid date, in the format DD/MM/YY hh:mm:ss.") }
    val notEmpty = { s: String => if(s.isEmpty) List("Value is required and can't be empty.") else Nil }
    val isSlug = { s: String => if(!s.matches("[a-z0-9]*")) List("Value can only contain lower-case alphanumeric characters.") else Nil }
    def notDuplicate(xs: List[String]) = { s: String => if(xs contains s) List("This value is not unique. Please choose something different.") else Nil }
  }

  trait FormLayout extends Form {
    implicit def renderContext[In]: Context[In, Html5.Element[Html5.Flow]] =
      new Context[In, Html5.Element[Html5.Flow]] {
        def action(formField: Form#FormField[In, Widget]): Html5.Element[Html5.Flow] = formField.render
      }
    
    def view: Html5.Element[Html5.Flow]
  }

  trait TabularLayout extends FormLayout {
    import Html5._
   
    def submitButton: Element[Flow] = input(`type` -> submit, name -> Symbol("_submit"), value -> submitButtonText)

    def submitButtonText = "Save"

    def submitRow(btn: Element[Flow]): Element[TrItems] = tr(
      td,
      td(btn)
    )

    def row(f: FormField[_, _]): Element[TrItems] =
      tr(
        td(f.label),
        td(f.render)
      )

    override def view = Html5.form(Html5.action -> this.action, Html5.method -> this.method.name)(
      table(
        tbody(
          formFields.map(row),
          submitRow(submitButton)
        )
      )
    )
  }
}
