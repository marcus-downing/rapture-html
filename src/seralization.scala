package rapture.html5

trait Serialization { this : Html5 =>

  def doSerialize(block : Boolean, name : String, attributes : Map[String, String],
      body : List[Element[_]], sb : StringBuilder, n : Int, indent : Boolean) =
    if(block) {
      if(!indent) sb.append("\n")
      sb.append("  "*n)
      sb.append("<")
      sb.append(name)
      for((k, v) <- attributes) {
        sb.append(" ")
        sb.append(k)
        if(v != "") {
          sb.append("=\"")
          sb.append(v)
          sb.append("\"")
        }
      }
      body.toList match {
        case Nil =>
          sb.append("/>")
        case h :: t =>
          sb.append(">\n")
          h.serialize(sb, n + 1, true)
          t.foreach(_.serialize(sb, n + 1, false))
          sb.append("\n")
          sb.append("  "*n)
          sb.append("</")
          sb.append(name)
          sb.append(">")
      }
    } else {
      if(indent) sb.append("  "*n)
      sb.append("<")
      sb.append(name)
      for((k, v) <- attributes) {
        sb.append(" ")
        if(v != "") {
          sb.append(k)
          sb.append("=\"")
          sb.append(v)
          sb.append("\"")
        }
      }
      sb.append(">")
      body.foreach(_.serialize(sb, n, false))
      sb.append("</")
      sb.append(name)
      sb.append(">")
    }
}

