//==========================================================================
// HEROE
//==========================================================================
case class Equipamiento(
                  cabeza: Item,
                  torso: Item,
                  manos: List[Item],
                  talismanes: List[Item]
                  ){
  require(manos.size <= 2, "Solo hay dos manos!")

  def agregarItem(item: Item) : Equipamiento = {
    item.cuerpoHeroe match {
      case Cabeza => copy(cabeza = item)
      case Torso => copy(torso = item)
      case Mano => copy(manos = manos.tail.appended(item)) // ver tema de armas que  ocupan dos manos
      case Talisman => copy(talismanes = talismanes.appended(item))
    }
  }

  def items : List[Item] = List(List(cabeza, torso), manos, talismanes).flatten
  def incrementoHP: Int = items.map(item=>item.incrementos.HP).sum
  def incrementoVelocidad: Int = items.map(item=>item.incrementos.velocidad).sum
  def incrementoInteligencia: Int = items.map(item=>item.incrementos.inteligencia).sum
  def incrementoFuerza: Int = items.map(item=>item.incrementos.fuerza).sum
}

case class Heroe(stats : Stats, inventario : List[Item], equipamiento: Equipamiento, trabajo : Trabajo) {

  def HP: Int = stats.HP.value + trabajo.incrementos.HP + equipamiento.incrementoHP

  def velocidad: Int = stats.velocidad.value + trabajo.incrementos.velocidad + equipamiento.incrementoVelocidad

  def inteligencia: Int = stats.inteligencia.value + trabajo.incrementos.inteligencia + equipamiento.incrementoInteligencia

  def fuerza: Int = stats.fuerza.value + trabajo.incrementos.fuerza + equipamiento.incrementoFuerza

  def convertirseEn(nuevoTrabajo: Trabajo): Heroe = {
    copy(trabajo = nuevoTrabajo)
  }

  def equiparseCon(item: Item): Heroe = {
    copy(equipamiento = equipamiento.agregarItem(item))
  }

}
  //==========================================================================
  // Trabajo
  //==========================================================================


  //==========================================================================
  // Inventario
  //==========================================================================
