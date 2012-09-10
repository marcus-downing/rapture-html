package rapture.html

object Test extends App {

  import Forms._

  def form(map: Map[String, String] = Map()) = new Form('myForm, map) with TabularLayout {
    val firstName = formField[String, StringInput]('firstName, "First name")
    val lastName = formField[String, StringInput]('lastName, "Last name")
    val address = formField[String, Textbox]('address, "Address")
    val male = formField[Boolean, Checkbox]('male, "Are you male?")
  }

  val f = form()
  println(f.view)

  val f2 = form(Map("myForm_firstName" -> "Jon", "myForm_lastName" -> "Pretty"))
  println(f2.view)

}
