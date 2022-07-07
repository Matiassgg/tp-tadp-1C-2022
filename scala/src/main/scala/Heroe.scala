//==========================================================================
// HEROE
//==========================================================================
case class Item(cuerpoHeroe: CuerpoHeroe, incrementos: Incrementos, restriccion: Heroe => Boolean) {
}

trait Trabajo {
  def statPrincipal: Heroe => Int
  def aumentarStats: Heroe => Heroe = (h: Heroe) => h.setStat(h.stats)
}

case object Guerrero extends Trabajo {
  def statPrincipal: Heroe => Int = _.stats.fuerza
  override def aumentarStats: Heroe => Heroe = {
    super.aumentarStats andThen(_.cambiarHP(10).cambiarFuerza(15).cambiarInteligencia(-10))
  }
}
case object Mago extends Trabajo {
  def statPrincipal: Heroe => Int = _.stats.inteligencia
}
case object Ladron extends Trabajo {
  def statPrincipal: Heroe => Int = _.stats.velocidad
}

//class Trabajo(val statPrincipal: Int, clase: TipoTrabajo, val incrementos: Incrementos) {
//
//}


//case class Incremento (HP: Int = 0, inteligencia: Int = 0, fuerza: Int = 0, velocidad: Int = 0)
case class Stats (HP: Int, inteligencia: Int, fuerza: Int, velocidad: Int) {
  require(HP > 1, "HP debe ser mayor a 1")
  require(inteligencia > 1, "inteligencia debe ser mayor a 1")
  require(fuerza > 1, "fuerza debe ser mayor a 1")
  require(velocidad > 1, "velocidad debe ser mayor a 1")

  def sumarAtributo(v1: Int, v2: Int) = (v1+v2).max(1)

  def cambiarHP(valor: Int) = copy(HP= sumarAtributo(HP, valor))
  def cambiarFuerza(valor: Int) = copy(fuerza= sumarAtributo(fuerza, valor))
  def cambiarInteligencia(valor: Int) = copy(inteligencia= sumarAtributo(inteligencia, valor))
}




sealed trait CuerpoHeroe
case object Cabeza extends CuerpoHeroe
case object Torso extends CuerpoHeroe
case object Mano extends CuerpoHeroe
case object Talisman extends CuerpoHeroe  // Tal vez Cuello en vez de Talisman

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

  def calcularIncrementos(heroe: Heroe): Heroe = ???
}

case class Heroe(stats: Stats, inventario : List[Item], equipamiento: Equipamiento, trabajo : Option[Trabajo]) {

  val statPrincipal: Int = trabajo.map(_.statPrincipal(this)).getOrElse(0)
  def statsConIncrementos: Stats = {
    val heroeConIncrementos = trabajo match {
      case Some(unTrabajo) => unTrabajo.aumentarStats(this)
      case None => this
    }

    equipamiento.calcularIncrementos(heroeConIncrementos).stats
  }
  def HP = statsConIncrementos.HP
  //def HP: Int = stats.HP + trabajo.incrementos.HP + equipamiento.incrementoHP

  def velocidad = statsConIncrementos.velocidad

  def cambiarHP(valor : Int) = copy(stats = stats.cambiarHP(valor))
  def cambiarFuerza(valor: Int) = copy(stats = stats.cambiarFuerza(valor))
  def cambiarInteligencia(valor: Int) = copy(stats = stats.cambiarInteligencia(valor))
  //def velocidad: Int = stats.velocidad + trabajo.incrementos.velocidad + equipamiento.incrementoVelocidad

  //def inteligencia: Int = stats.inteligencia + trabajo.incrementos.inteligencia + equipamiento.incrementoInteligencia

  //def fuerza: Int = stats.fuerza + trabajo.incrementos.fuerza + equipamiento.incrementoFuerza

  def convertirseEn(nuevoTrabajo: Trabajo): Heroe = copy(trabajo = Some(nuevoTrabajo))

  def equiparseCon(item: Item): Heroe =
    copy(equipamiento = equipamiento.agregarItem(item))


  def renunciar = copy(trabajo = None)

  def setStat(stat: Stats) = copy(stats = stat)
}
  //==========================================================================
  // Trabajo
  //==========================================================================


  //==========================================================================
  // Inventario
  //==========================================================================
