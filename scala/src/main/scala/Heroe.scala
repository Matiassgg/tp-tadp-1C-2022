//==========================================================================
// STATS
//==========================================================================

case class Incrementos (HP: Int = 0, inteligencia: Int = 0, fuerza: Int = 0, velocidad: Int = 0)

case class Stats (HP: Int, inteligencia: Int, fuerza: Int, velocidad: Int) {
  require(HP > 1, "HP debe ser mayor a 1")
  require(inteligencia > 1, "inteligencia debe ser mayor a 1")
  require(fuerza > 1, "fuerza debe ser mayor a 1")
  require(velocidad > 1, "velocidad debe ser mayor a 1")

  def sumarAtributo(v1: Int, v2: Int) = (v1+v2).max(1)
  def cambiarHP(valor: Int) = copy(HP= sumarAtributo(HP, valor))
  def cambiarFuerza(valor: Int) = copy(fuerza= sumarAtributo(fuerza, valor))
  def cambiarInteligencia(valor: Int) = copy(inteligencia= sumarAtributo(inteligencia, valor))
  def cambiarVelocidad(valor: Int) = copy(velocidad= sumarAtributo(velocidad, valor))
}

//==========================================================================
// TRABAJO
//==========================================================================

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

//==========================================================================
// ITEMS
//==========================================================================

type Restriccion = Heroe => Boolean

case class Item(zonaEquipamiento: ZonaEquipamiento, incrementos: Incrementos, restricciones: List[Restriccion] = List(), dosManos: Boolean = false) {
  require(!(dosManos && !(zonaEquipamiento == Mano)), "Como va a requerir dos manos si no es un item para manos!")
}

case object CascoVikingo extends Item(Cabeza, Incrementos(10,0,0,0), List(((h: Heroe) => h.fuerzaBase > 30)))
case object PalitoMagico extends Item(Mano, Incrementos(0,20,0,0), List(((h: Heroe) => h.esMago || (h.esLadron && h.inteligenciaBase > 30))))
case object ArmaduraEleganteSport extends Item(Torso, Incrementos(-30,0,0,30))
case object ArcoViejo extends Item(Mano, Incrementos(0,0,2,0), List(), true)
case object EscudoAntiRobo extends Item(Mano, Incrementos(20,0,0,0), List( (h: Heroe) => !h.esLadron, (h: Heroe) => h.fuerzaBase > 20), false)
case object TalismanDeDedicacion extends Item(Talisman, Incrementos()) // TODO:  Todos los stats se incrementan 10% del valor del stat principal del trabajo.
case object TalismanDelMinimalismo extends Item(Talisman, Incrementos(50)) // TODO: +50 hp. -10 hp por cada otro ítem equipado.

// TODO: Si el héroe tiene más fuerza que inteligencia, +30 a la inteligencia; de lo contrario +10 a todos los stats menos la inteligencia.
case object VinchaDelBufaloDeAgua extends Item(Cabeza, Incrementos(), List( (h: Heroe) => h.esDesempleado ))

case object TalismanMaldito extends Item(Talisman, Incrementos()) // Reduce todos los stats a 1.

case object EspadaDeLaVida extends Item(Mano, Incrementos()) // Hace que la fuerza del héroe sea igual a su hp.

//==========================================================================
// INVENTARIO
//==========================================================================

sealed trait ZonaEquipamiento
case object Cabeza extends ZonaEquipamiento
case object Torso extends ZonaEquipamiento
case object Mano extends ZonaEquipamiento
case object Talisman extends ZonaEquipamiento  // Tal vez Cuello en vez de Talisman

case class Inventario(
                  cabeza: Item,
                  torso: Item,
                  manos: List[Item],
                  talismanes: List[Item]
                  ){
  require(manos.size <= 2, "Solo hay dos manos!")

  def agregarItem(item: Item) : Inventario = {

    item.zonaEquipamiento match {
      case Cabeza => copy(cabeza = item)
      case Torso => copy(torso = item)
      case Mano => item.dosManos match {
        case true => copy(manos = List(item))
        case false => {
          manos.head.dosManos match {
            case true => copy(manos = List(item))
            case false => if(manos.size == 2) copy(manos = manos.tail.appended(item)) else copy(manos = manos.appended(item))
          }
        }
      }
      case Talisman => copy(talismanes = talismanes.appended(item))
    }
  }

/*  case class Equipamiento(
                  cabeza: Option[Item],
                  torso: Option[Item],
                  manoIzq: Option[Item],
                  manoDer: Option[Item],
                  ambasManos: Option[Item],
                  talismanes: List[Item]
                  ){
    require(!((ambasManos != None && manoDer == None && manoIzq == None)), "Si ambasManos tiene valores, manoDer y manoIzq no pueden tenerlos")
    require(!((manoDer != None || manoIzq != None) && ambasManos == None), "Si alguna mano tiene valores, ambasManos debe ser None")

  def agregarItem(item: Item) : Equipamiento = {

    item.cuerpoHeroe match {
      case Cabeza => copy(cabeza = Some(item))
      case Torso => copy(torso = Some(item))
      case Mano => item.dosManos match {
        case true => {
          copy(ambasManos = Some(item), manoIzq = None, manoDer = None)
        }
        case false => {

        }
      } // ver tema de armas que  ocupan dos manos
      case Talisman => copy(talismanes = talismanes.appended(item))
    }
  }*/

  def items : List[Item] = List(List(cabeza, torso), manos, talismanes).flatten
  def incrementoHP: Int = items.map(item=>item.incrementos.HP).sum
  def incrementoVelocidad: Int = items.map(item=>item.incrementos.velocidad).sum
  def incrementoInteligencia: Int = items.map(item=>item.incrementos.inteligencia).sum
  def incrementoFuerza: Int = items.map(item=>item.incrementos.fuerza).sum

  def calcularIncrementos(heroe: Heroe): Heroe = heroe.cambiarHP(incrementoHP)
                                                      .cambiarVelocidad(incrementoVelocidad)
                                                      .cambiarFuerza(incrementoFuerza)
                                                      .cambiarInteligencia(incrementoInteligencia)
}

//==========================================================================
// HEROE
//==========================================================================

case class Heroe(stats: Stats, inventario: Inventario, trabajo : Option[Trabajo]) {

  val statPrincipal: Int = trabajo.map(_.statPrincipal(this)).getOrElse(0)
  def statsConIncrementos: Stats = {
    val heroeConIncrementos = trabajo match {
      case Some(unTrabajo) => unTrabajo.aumentarStats(this)
      case None => this
    }

    inventario.calcularIncrementos(heroeConIncrementos).stats
  }

  //Stats base
  def hpBase = stats.HP
  def fuerzaBase = stats.fuerza
  def velocidadBase = stats.velocidad
  def inteligenciaBase = stats.inteligencia

  //Stats con buffs
  def HP = statsConIncrementos.HP
  def velocidad = statsConIncrementos.velocidad
  def inteligencia = statsConIncrementos.inteligencia
  def fuerza = statsConIncrementos.fuerza

  //Stats Setters
  def cambiarHP(valor : Int) = copy(stats = stats.cambiarHP(valor))
  def cambiarFuerza(valor: Int) = copy(stats = stats.cambiarFuerza(valor))
  def cambiarInteligencia(valor: Int) = copy(stats = stats.cambiarInteligencia(valor))
  def cambiarVelocidad(valor: Int) = copy(stats = stats.cambiarVelocidad(valor))


  def convertirseEn(nuevoTrabajo: Trabajo): Heroe = copy(trabajo = Some(nuevoTrabajo))

  def equiparseCon(item: Item): Heroe =
    if (item.restricciones.forall( r => r(this)))
      copy (inventario = inventario.agregarItem (item) )
    else this

  def renunciar = copy(trabajo = None)

  def setStat(stat: Stats) = copy(stats = stat)

  def esMago = trabajo match {
    case Some(Mago) => true
    case _ => false
  }

  def esGuerrero = trabajo match {
    case Some(Guerrero) => true
    case _ => false
  }

  def esLadron = trabajo match {
    case Some(Ladron) => true
    case _ => false
  }

  def esDesempleado = trabajo match {
    case None => true
    case _ => false
  }
}

//==========================================================================
// Equipo
//==========================================================================

case class Equipo(integrantes: Set[Heroe]){

}