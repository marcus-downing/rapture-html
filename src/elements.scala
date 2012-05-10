package rapture.html

trait ElementClasses { this : Html5 =>

  trait ElementType
  trait Flow extends ElementType
  trait Sectioning extends Flow
  trait Heading extends Flow
  trait Metadata extends ElementType
  trait Phrasing extends Flow
  trait Embedded extends Phrasing
  trait Interactive extends Flow
  trait Text extends Phrasing
  trait Top extends ElementType
  trait Definitions extends ElementType
  trait ListItems extends ElementType
  trait TableItems extends ElementType
  trait ColItems extends ElementType
  trait TrItems extends ElementType
  trait TdItems extends ElementType
  trait OptionItems extends ElementType

}

