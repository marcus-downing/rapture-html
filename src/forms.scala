package rapture.html

import rapture.io._
import scala.collection.mutable.ListBuffer

import rapture.orm.Time._

object Forms extends Widgets with Parsers {

  class BasicForm(val name: Symbol, val params: Map[String, String] = Map(),
      val uploads: Map[String, Array[Byte]] = Map()) { form =>
    
    type Field[T] <: BasicField[T]
    
    val formName = name.name
    protected val fields = new ListBuffer[Field[_]]

    def submitted = params.contains(formName+"_submit")
    def complete = submitted
    def save() = fields.foreach(_.save())

    trait BasicField[T] {
      def name: Symbol
      def fieldName: String = name.name
      def paramValue: Option[String] = form.params.get(fieldName)
      
      def value: Option[T] =
        if(parser.submitted(stringValue)) Some(parser.parse(stringValue, dataValue)) else None
      
      def dataValue: Option[Array[Byte]] = form.uploads.get(fieldName)

      def stringValue: Option[String] =
        if(form.submitted) paramValue else if(cell == null) None else parser.serialize(cell())
      
      def fieldValue: String = stringValue.getOrElse("")
      def apply(): T = value.get
      def parser: Parser[T]
      def cell: Cell[T]
      def save(): Unit = value foreach { c => if(cell == null) () else cell.update(c) }
    }
  }

  trait FieldLabels {
    type FormField[T] <: LabelledField
    trait LabelledField { def label: String }
  }

  trait FormValidation { this: (BasicForm with FormValidation) =>
    
    def validated = fields.forall(_.validated)
    def showValidation = submitted && !validated

    abstract override def complete = submitted && validated

    type Field[T] <: ValidatedField[T]
    trait ValidatedField[T] { this: BasicField[T] =>
      lazy val validationIssues: List[String] =
        if(!submitted) Nil else stringValue.map(validator).getOrElse(Nil)
      def validated: Boolean = validationIssues.isEmpty
      def validator: String => List[String]
      def required: Boolean
    }
 
    // String validators

    val validUrl = { s: String => if(s.matches("\\b(https?|ftp)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a"+
        "-zA-Z0-9+&@#/%=~_|]")) Nil else List("Please enter a valid URL") }

    val validPhoneNumber = { s: String => if(s.matches("""^[+\- ()0-9]*$""")) Nil else List("Pleas"+
        "e enter a valid telephone number") }
    
    val validEmailAddress = { s: String => if(s.matches("""^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+"+
        "(\.[a-z0-9-]+)*(\.[a-z]{2,4})$""")) Nil else List("Please enter a valid email address") }
    
    val optValidEmailAddress = { s: String => if(s.matches("""^([_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-"+
        "9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4}))?$""")) Nil else List("Please enter a valid email addre"+
        "ss") }
    
    val validDateTime = { s: String => if(s.matches("[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9] [0-9][0-"+
        "9]:[0-9][0-9]:[0-9][0-9]")) Nil else List("Please enter a valid date, in the format DD/MM"+
        "/YY hh:mm:ss.") }
    
    val notEmpty = { s: String => if(s.isEmpty) List("Value is required and can't be empty.") else
        Nil }
    
    val isSlug = { s: String => if(!s.matches("[a-z0-9]*")) List("Value can only contain lower-cas"+
        "e alphanumeric characters.") else Nil }
    
    def notDuplicate(xs: List[String]) = { s: String => if(xs contains s) List("This value is not "+
        "unique. Please choose something different.") else Nil }
  
  }

  trait FormHelp { this: BasicForm =>
    type Field[T] <: HelpField[T]
    trait HelpField[T] extends BasicField[T] { def help: String }
  }

  trait Preprocessing { this: BasicForm =>

    type Field[T] <: PreprocessedField[T]

    trait PreprocessedField[T] extends BasicField[T] {
      def processString(s: String): String
      override def stringValue = super.stringValue.map(processString)
    }
  }

  /** Adds renderability functionality to a form.  */
  trait RenderableForm { this: (BasicForm with RenderableForm) =>
    type Field[T] <: RenderableField[T]
    type RenderType
    type FormPart
    type RenderedForm
    val formParts = new ListBuffer[FormPart]

    def wrap[T, F <: Field[T], W <: Widget[T]](field: F, widget: W)
        (implicit renderer: Renderer[T, F, W]): FormPart

    def content(fp: FormPart) = formParts += fp

    def render: RenderedForm

    // asInstanceOf[F] is here as an indirect consequence of compiler bug SI-6443
    trait RenderableField[T] { this: Field[T] =>
      def as[F <: Field[T], W <: Widget[T]](w: W)(implicit renderer:
          Renderer[T, F, W]): this.type = {
        formParts += wrap[T, F, W](this.asInstanceOf[F], w)(renderer)
        fields += this
        this
      }
    }

    trait Renderer[T, -F <: RenderableField[T], -W <: Widget[T]] {
      def render(f: F, w: W): RenderType
    }
  }

  abstract class WebForm(name: Symbol, params: Map[String, String] = Map(),
      uploads: Map[String, Array[Byte]] = Map(),
      val method: HttpMethods.FormMethod = HttpMethods.Post, val action: Link = ^) extends
      BasicForm(name, params, uploads) with RenderableForm with FieldLabels with Preprocessing with
      FormValidation with FormHelp {

    class Field[T](val name: Symbol, val label: String, val cell: Cell[T], val parser: Parser[T],
        process: String => String, validate: String => List[String], val required: Boolean,
        val help: String) extends BasicField[T] with RenderableField[T] with LabelledField with
        PreprocessedField[T] with ValidatedField[T] with HelpField[T] {
      def processString(s: String) = process(s)
      def validator = validate
    }

    def field[T: Parser](name: Symbol, label: String, cell: Cell[T] = null,
        process: (String => String) = identity[String], validate: String => List[String] = { s =>
        Nil }, required: Boolean = false, help: String = "") =
      new Field[T](name, label, cell, implicitly[Parser[T]], process, validate, required, help)
    
    import Html5._

    type RenderType = Element[Phrasing]

    implicit val stringRenderer = new Renderer[String, Field[String], StringInput[String]] {
      def render(f: Field[String], w: StringInput[String]): Html5.Element[Html5.Phrasing] =
        input(Html5.name -> f.name, Html5.value -> f.fieldValue)
    }

    implicit val uploadRenderer = new Renderer[FileUrl, Field[FileUrl], FileUploader[FileUrl]] {
      def render(f: Field[FileUrl], w: FileUploader[FileUrl]): Html5.Element[Html5.Phrasing] =
        input(Html5.name -> f.name, Html5.`type` -> Html5.file, Html5.value -> f.fieldValue)
    }

    implicit val checkboxRenderer = new Renderer[Boolean, Field[Boolean], Checkbox[Boolean]] {
      def render(f: Field[Boolean], w: Checkbox[Boolean]): Html5.Element[Html5.Phrasing] =
        input(Html5.`type` -> checkbox, Html5.name -> f.name, if(f.value.getOrElse(false))
            Some(checked) else None)
    }

    implicit val textareaRenderer = new Renderer[String, Field[String], TextArea[String]] {
      def render(f: Field[String], w: TextArea[String]): Html5.Element[Html5.Phrasing] =
        textarea(Html5.name -> f.name)(f.fieldValue)
    }
    
    implicit def radioListRenderer[T] = new Renderer[T, Field[T], RadioList[T]] {
      def render(f: Field[T], w: RadioList[T]): Html5.Element[Html5.Phrasing] =
        span(style -> "display: inline-block")(
          w.options flatMap { opt => List(
            span(
              input(`type` -> radio, Html5.name -> f.name, value -> w.id(opt),
                  if(w.id(opt) == f.fieldValue) Some(checked) else None),
              " "+w.description(opt),
              br
            )
          ) }
        )
    }
  }

  trait TabularLayout { this: (WebForm with TabularLayout) =>

    import Html5._

    type FormPart = Element[TrItems]
    type RenderedForm = Element[Flow]

    def hideLabels = false

    def wrap[T, F <: Field[T], W <: Widget[T]](field: F, widget: W)
        (implicit renderer: Renderer[T, F, W]): FormPart =
      tr(
        if(hideLabels) Nil else List(
          td(field.label),
          td(if(field.required) "*" else "")
        ),
        td(renderer.render(field, widget))
      )

    def render: RenderedForm =
      form(Html5.action -> action, Html5.method -> method)(
        table(
          tbody(
            formParts.toList,
            submitRow
          )
        )
      )
   
    def submitButtonText = "Save"

    def submitRow = if(hideLabels) tr(td(submitButton)) else tr(
      td,
      td,
      td(submitButton)
    )

    def submitButton: Element[Flow] =
      input(Html5.name -> Symbol(formName+"_submit"), value -> submitButtonText, `type` -> submit)

  }
}

trait Parsers {
  trait Parser[Value] {
    def parse(value: Option[String], data: Option[Array[Byte]] = None): Value
    def serialize(value: Value): Option[String]
    def submitted(value: Option[String]): Boolean = value.isDefined
  }

  implicit val StringParser = new Parser[String] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = s.getOrElse("")
    def serialize(s: String): Option[String] = Some(s)
  }

  implicit val IntParser = new Parser[Int] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None): Int = s.get.toInt
    def serialize(value: Int) = Some(value.toString)
  }

  implicit val BooleanParser = new Parser[Boolean] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = s.isDefined
    def serialize(value: Boolean) = if(value) Some("") else None
    override def submitted(value: Option[String]): Boolean = true
  }

  implicit val DataParser = new Parser[Array[Byte]] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = data.getOrElse(Array[Byte]())
    def serialize(value: Array[Byte]) = Some("")
  }

  def enumParser(enum: Enumeration) = new Parser[enum.Value] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = enum(s.get.toInt)
    def serialize(value: enum.Value) = Some(value.id.toString)
  }
}

