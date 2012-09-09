package rapture.html

import rapture.io._
import scala.collection.mutable._

object Forms {

  trait Context[T, Out] {
    def action(value: String): Out
  }
  trait Widget[W] { def render(value: String): Html5.Element[Html5.Flow] }
  trait Parser[T] { def parse(value: String): T }

  implicit def context[T](implicit parser: Parser[T]): Context[T, T] =
    new Context[T, T] {
      def action(value: String): T = parser.parse(value)
    }

  case class Method(name: String)
  val Get = Method("get")
  val Post = Method("post")

  implicit val parser = new Parser[String] { def parse(s: String) = s }

  abstract class Form(name: Symbol, params: Map[String, String] = Map(), val method: Method = Post,
      val action: Path = ^) {

    protected val formFields: ListBuffer[Field[_]] = new ListBuffer[Field[_]]
    val formName = name.name
    def view: Html5.Element[Html5.Flow]
   
    def formField[T, W](name: Symbol, label: String)(implicit parser: Parser[T],
        renderer: Widget[W]): W

    abstract class Field[S](label: String, name: Symbol)(implicit parser: Parser[S]) {
      val fieldName: String = name.name
      def value(params: Map[String, String]): S
      def render: Html5.Element[Html5.Flow]
      def apply[T, Out]()(implicit n: Context[T, Out]): Out =
        n.action(params(formName+"_"+fieldName))

      formFields += this
    }
  }

  trait FormLayout extends Form {
    implicit def renderContext[W](implicit widget: Widget[W]): Context[Widget[W], Html5.Element[Html5.Flow]] =
      new Context[Widget[W], Html5.Element[Html5.Flow]] {
        type Out = Html5.Element[Html5.Flow]
        def action(value: String): Html5.Element[Html5.Flow] = widget.render(value)
      }
    
    def view: Html5.Element[Html5.Flow]
  }

  trait TabularLayout extends FormLayout {

    override def view = Html5.form(Html5.action -> action, Html5.method -> method.name)(
      Html5.table(
        Html5.tbody(
          formFields map { f => Html5.tr(Html5.td(f.render)) }
        )
      )
    )
  }

  /*val testForm = new Form('testForm) with TabularLayout { form =>
    val firstName = formField[String, Form#Field]('firstName, "First name")
  
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

  val n: String = testForm.firstName()*/

}
