case class Restriccion(operacionDeRestriccion : RestrictionOperation) {
  def verificar(heroe : Heroe) : Boolean = {
    operacionDeRestriccion.apply(heroe)
  }
}
