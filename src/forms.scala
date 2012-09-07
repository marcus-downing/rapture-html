package rapture.html

import rapture.io._
import scala.collection.mutable._

object Forms {

  trait Nature[T, Out] {
    def action(value: String): Out
  }
  trait Widget[W] { def render(value: String): Html5.Element[Html5.Flow] }
  trait Parser[T] { def parse(value: String): T }

  implicit def nature[T](implicit parser: Parser[T]): Nature[T, T] =
    new Nature[T, T] {
      def action(value: String): T = parser.parse(value)
    }

  case class Method(name: String)
  val Get = Method("get")
  val Post = Method("post")

  implicit val parser = new Parser[String] { def parse(s: String) = s }

  abstract class Form(name: Symbol, params: Map[String, String] = Map(), val method: Method = Post,
      val action: Path = ^) {

    protected val fields: ListBuffer[Field[_]] = new ListBuffer[Field[_]]
    val formName = name.name
    def view: Html5.Element[Html5.Flow]
   
    def formField[T, W](name: Symbol, label: String)(implicit parser: Parser[T],
        renderer: Widget[W]) = ()

    abstract class Field[S](label: String, name: Symbol)(implicit parser: Parser[S]) {
      val fieldName: String = name.name
      def value(params: Map[String, String]): S
      def render: Html5.Element[Html5.Flow]
      def apply[T, Out]()(implicit n: Nature[T, Out]): Out =
        n.action(params(formName+"_"+fieldName))

      fields += this
    }

    /*case class StringInput(label: String, name: Symbol) extends Field[String](label, name) {
      def value(params: Map[String, String]): String = params(formName+"_"+fieldName)
      def render = Html5.input
    }

    case class IntInput(label: String, name: Symbol) extends Field[Int](label, name) {
      def value(params: Map[String, String]): Int = params(formName+"_"+fieldName).toInt
      def render = Html5.input
    }*/
  }

  trait FormLayout extends Form {
    implicit def renderNature[W](implicit widget: Widget[W]): Nature[Widget[W], Html5.Element[Html5.Flow]] =
      new Nature[Widget[W], Html5.Element[Html5.Flow]] {
        type Out = Html5.Element[Html5.Flow]
        def action(value: String): Html5.Element[Html5.Flow] = widget.render(value)
      }
    
    def view: Html5.Element[Html5.Flow]
  }

  trait TabularLayout extends FormLayout {

    override def view = Html5.form(Html5.action -> action, Html5.method -> method.name)(
      Html5.table(
        Html5.tbody(
          fields map { f => Html5.tr(Html5.td(f.render)) }
        )
      )
    )
  }

  val testForm = new Form('testForm) with TabularLayout { form =>
    val firstName = formField[String, Input]('firstName, "First name")
  
    override def view = {
      import Html5.{form => _, _}
      Html5.form(Html5.action -> form.action)(
        firstName()
      )
    }
  }

  implicit def textInput[W](implicit p: Parser[W]) = new Widget[W] {
    def render(v: String) = Html5.input(Html5.value -> v)
  }

  val n: String = testForm.firstName()

}
