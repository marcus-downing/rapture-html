package rapture.html

import rapture.io._

object Forms {

  case class Method(name: String)
  val Get = Method("get")
  val Post = Method("post")

  abstract class Form(name: Symbol, params: Map[String, String] = Map(), val method: Method = Post,
      val action: Path = ^) {

    val formName = name.name
    def view: Html5.Element[_]
    
    abstract class Field[S](name: Symbol) {
      val fieldName: String = name.name
      def value(params: Map[String, String]): S
    }

    case class StringInput(name: Symbol) extends Field[String](name) {
      def value(params: Map[String, String]): String = params(formName+"_"+fieldName)
    }

  }

  trait FormLayout extends Form {
    def view: Html5.Element[_]
  }

  trait TabularLayout extends FormLayout {
    override def view = Html5.form(Html5.action -> action, Html5.method -> method.name)
  }

  val testForm = new Form('testForm) with TabularLayout {
    val firstName = new StringInput('firstName)
  }

}
