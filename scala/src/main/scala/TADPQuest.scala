object TADPQuest {
  //==========================================================================
  // STATS
  //==========================================================================

  case class Incrementos (HP: Int = 0, inteligencia: Int = 0, fuerza: Int = 0, velocidad: Int = 0)

  case class Stats (HP: Int, inteligencia: Int, fuerza: Int, velocidad: Int) {
    require(HP > 1, "HP debe ser mayor a 1")
    require(inteligencia > 1, "inteligencia debe ser mayor a 1")
    require(fuerza > 1, "fuerza debe ser mayor a 1")
    require(velocidad > 1, "velocidad debe ser mayor a 1")

    def sumarAtributo(atributo1: Int, atributo2: Int) : Int = (atributo1+atributo2).max(1)
    def cambiarHP(valor: Int) : Stats = copy(HP = sumarAtributo(HP, valor))
    def cambiarFuerza(valor: Int) : Stats = copy(fuerza = sumarAtributo(fuerza, valor))
    def cambiarInteligencia(valor: Int) : Stats = copy(inteligencia = sumarAtributo(inteligencia, valor))
    def cambiarVelocidad(valor: Int) : Stats = copy(velocidad = sumarAtributo(velocidad, valor))

    def recalcularStats(incrementos: Incrementos) = copy(HP = HP + incrementos.HP,
                                                               inteligencia = inteligencia + incrementos.inteligencia,
                                                               fuerza = fuerza + incrementos.fuerza,
                                                               velocidad = velocidad + incrementos.velocidad)
  }

  //==========================================================================
  // TRABAJO
  //==========================================================================

  trait Trabajo {
    def statPrincipal: Heroe => Int
    def aumentarStats: Heroe => Heroe = (h: Heroe) => h.setStat(h.stats)
  }

  case object Guerrero extends Trabajo {
    def statPrincipal: Heroe => Int = _.fuerza
    override def aumentarStats: Heroe => Heroe = {
      super.aumentarStats andThen(_.cambiarHP(10).cambiarFuerza(15).cambiarInteligencia(-10))
    }
  }
  case object Mago extends Trabajo {
    def statPrincipal: Heroe => Int = _.inteligencia
  }
  case object Ladron extends Trabajo {
    def statPrincipal: Heroe => Int = _.velocidad // Se toman los stats con incrementos
  }

  //==========================================================================
  // ITEMS
  //==========================================================================

  type Restriccion = Heroe => Boolean

  // Primer approach
    /*
    case class Item(zonaEquipamiento: ZonaEquipamiento, incrementos: Incrementos, restricciones: List[Restriccion] = List(), dosManos: Boolean = false) {
      require(!(dosManos && !(zonaEquipamiento == Mano)), "Como va a requerir dos manos si no es un item para manos!")
    }

      case object CascoVikingo extends Item(Cabeza, Incrementos(10,0,0,0), List((h: Heroe) => h.fuerzaBase > 30))
      case object PalitoMagico extends Item(Mano, Incrementos(0,20,0,0), List((h: Heroe) => h.esMago || (h.esLadron && h.inteligenciaBase > 30)))
      case object ArmaduraEleganteSport extends Item(Torso, Incrementos(-30,0,0,30))
      case object ArcoViejo extends Item(Mano, Incrementos(0,0,2,0), List(), true)
      case object EscudoAntiRobo extends Item(Mano, Incrementos(20,0,0,0), List( (h: Heroe) => !h.esLadron, (h: Heroe) => h.fuerzaBase > 20), false)
      case object TalismanDeDedicacion extends Item(Talisman, Incrementos()) // TODO:  Todos los stats se incrementan 10% del valor del stat principal del trabajo.
      case object TalismanDelMinimalismo extends Item(Talisman, Incrementos(50)) // TODO: +50 hp. -10 hp por cada otro ítem equipado.

      // TODO: Si el héroe tiene más fuerza que inteligencia, +30 a la inteligencia; de lo contrario +10 a todos los stats menos la inteligencia.
      case object VinchaDelBufaloDeAgua extends Item(Cabeza, Incrementos(), List( (h: Heroe) => h.esDesempleado ))

      case object TalismanMaldito extends Item(Talisman, Incrementos()) // Reduce todos los stats a 1.

      case object EspadaDeLaVida extends Item(Mano, Incrementos()) // Hace que la fuerza del héroe sea igual a su hp.
      */

  abstract case class Item(zonaEquipamiento: ZonaEquipamiento, restricciones: List[Restriccion] = List(), dosManos: Boolean = false, valorVenta: Int = 0) {
    require(!(dosManos && !(zonaEquipamiento == Mano)), "Como va a requerir dos manos si no es un item para manos!")

    def getStatsModificados(heroe : Heroe) : Stats

    def aplicarEfectoAHeroe(heroe: Heroe): Heroe = heroe.setStat(getStatsModificados(heroe))
  }

  case object TalismanDeDedicacion extends Item(Talisman,List.empty) {
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(heroe.statPrincipal*0.1.toInt,heroe.statPrincipal*0.1.toInt,heroe.statPrincipal*0.1.toInt,heroe.statPrincipal*0.1.toInt))
    }
  }

  case object TalismanDelMinimalismo extends Item(Talisman,List.empty) {
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(50-(10*heroe.cantidadItemsEquipados)))
    }
  }

  case object VinchaDelBufaloDeAgua extends Item(Cabeza, List( (h: Heroe) => h.esDesempleado )){
    override def getStatsModificados(heroe: Heroe): Stats = {
      val incrementos = if(heroe.fuerzaBase > heroe.inteligenciaBase) Incrementos(0,30,0,0) else Incrementos(10,0,10,10)
      heroe.stats.recalcularStats(incrementos)
    }
  }

  case object TalismanMaldito extends Item(Talisman) {
    override def getStatsModificados(heroe: Heroe): Stats = {
      Stats(1,1,1,1)
    }
  }

  case object EspadaDeLaVida extends Item(Mano) {
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(0,0,(heroe.fuerzaBase * -1) + heroe.hpBase,0))
    }
  }

  case object CascoVikingo extends Item(Cabeza, List((h: Heroe) => h.fuerzaBase > 30)){
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(10,0,0,0))
    }
  }

  case object PalitoMagico extends Item(Mano, List((h: Heroe) => h.esMago || (h.esLadron && h.inteligenciaBase > 30))){
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(0,20,0,0))
    }
  }

  case object ArmaduraEleganteSport extends Item(Torso){
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(-30,0,0,30))
    }
  }

  case object ArcoViejo extends Item(Mano, List(), true){
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(0,0,2,0))
    }
  }

  case object EscudoAntiRobo extends Item(Mano, List( (h: Heroe) => !h.esLadron, (h: Heroe) => h.fuerzaBase > 20), false){
    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats.recalcularStats(Incrementos(20,0,0,0))
    }
  }

  //==========================================================================
  // INVENTARIO
  //==========================================================================

  sealed trait ZonaEquipamiento
  case object Cabeza extends ZonaEquipamiento
  case object Torso extends ZonaEquipamiento
  case object Mano extends ZonaEquipamiento
  case object Talisman extends ZonaEquipamiento  // Tal vez Cuello en vez de Talisman

  case class Equipamiento(
                    cabeza: Item,
                    torso: Item,
                    manos: List[Item],
                    talismanes: List[Item]
                    ){
    require(manos.size <= 2, "Solo hay dos manos!")

    def agregarItem(item: Item) : Equipamiento = {

      item.zonaEquipamiento match {
        case Cabeza => copy(cabeza = item)
        case Torso => copy(torso = item)
        case Mano => if (item.dosManos) {
          copy(manos = List(item))
        } else {
          if (manos.head.dosManos) {
            copy(manos = List(item))
          } else {
            if (manos.size == 2) copy(manos = manos.tail.appended(item)) else copy(manos = manos.appended(item))
          }
        }
        case Talisman => copy(talismanes = talismanes.appended(item))
      }
    }

    def items : List[Item] = List(List(cabeza, torso), manos, talismanes).flatten

    //Testear si funca el fold
    def calcularIncrementos(heroe: Heroe) = items.foldLeft(heroe)((buffedHeroe, item) => item.aplicarEfectoAHeroe(buffedHeroe))
  }

  List(1,2,3,4).sum
  //==========================================================================
  // HEROE
  //==========================================================================

  case class Heroe(stats: Stats, inventario: List[Item], equipamiento: Equipamiento, trabajo : Option[Trabajo]) {

    val statPrincipal: Int = trabajo.map(_.statPrincipal(this)).getOrElse(0)
    def statsConIncrementos: Stats = {
      val heroeConIncrementos = trabajo match {
        case Some(unTrabajo) => unTrabajo.aumentarStats(this)
        case None => this
      }

      equipamiento.calcularIncrementos(heroeConIncrementos).stats
    }

    // Stats base
    def hpBase : Int = stats.HP
    def fuerzaBase : Int = stats.fuerza
    def velocidadBase : Int = stats.velocidad
    def inteligenciaBase : Int = stats.inteligencia

    // Stats con buffs
    def HP : Int = statsConIncrementos.HP
    def velocidad : Int = statsConIncrementos.velocidad
    def inteligencia : Int = statsConIncrementos.inteligencia
    def fuerza : Int = statsConIncrementos.fuerza

    // Stats Setters
    def cambiarHP(valor : Int) : Heroe = copy(stats = stats.cambiarHP(valor))
    def cambiarFuerza(valor: Int) : Heroe = copy(stats = stats.cambiarFuerza(valor))
    def cambiarInteligencia(valor: Int) : Heroe = copy(stats = stats.cambiarInteligencia(valor))
    def cambiarVelocidad(valor: Int) : Heroe = copy(stats = stats.cambiarVelocidad(valor))


    def convertirseEn(nuevoTrabajo: Trabajo): Heroe = copy(trabajo = Some(nuevoTrabajo))

    def equiparseCon(item: Item): Heroe =
      if (item.restricciones.forall( r => r(this)))
        copy (equipamiento = equipamiento.agregarItem (item) )
      else this

    def renunciar : Heroe = copy(trabajo = None)

    def setStat(stat: Stats) : Heroe = copy(stats = stat)

    def esMago : Boolean = trabajo match {
      case Some(Mago) => true
      case _ => false
    }

    def esGuerrero : Boolean = trabajo match {
      case Some(Guerrero) => true
      case _ => false
    }

    def esLadron : Boolean = trabajo match {
      case Some(Ladron) => true
      case _ => false
    }

    def esDesempleado : Boolean = trabajo match {
      case None => true
      case _ => false
    }

    // Inventario
    def cantidadItemsEquipados: Int = equipamiento.items.size

    def agregarAlInventario(item: Item): Heroe = copy(inventario = inventario.appended(item))
  }

  //==========================================================================
  // Equipo
  //==========================================================================

  case class Equipo(nombre: String, integrantes: Set[Heroe], pozoComun: Int = 0) {

    def mejorHeroeSegun(cuantificador: Heroe => Int): Option[Heroe] = integrantes.reduceOption((h1,h2) => if(cuantificador(h1) > cuantificador(h2)) h1 else h2)

    def obtenerItem(item: Item): Equipo = {
      implicit def diferenciaStatPrincipal(integrante: Heroe, item: Item) : Int = integrante.statPrincipal - integrante.equiparseCon(item).statPrincipal
      val integrantesBeneficiados = integrantes.filter(integrante => diferenciaStatPrincipal(integrante, item) > 0)

      if (integrantesBeneficiados.nonEmpty) {
        val heroeMasBeneficiado : Heroe = integrantes.maxBy(integrante => diferenciaStatPrincipal(integrante, item))
        reemplazarMiembro(heroeMasBeneficiado, heroeMasBeneficiado.equiparseCon(item))
      }
      else
        venderItem(item)
    }

    def obtenerMiembro(heroe: Heroe): Equipo = copy(integrantes = integrantes + heroe)

    def reemplazarMiembro(heroeReemplazado: Heroe, heroeNuevo: Heroe) : Equipo = copy(integrantes = integrantes - heroeReemplazado + heroeNuevo)

    def lider: Option[Heroe] = {
      val liderPotencial: Option[Heroe] = integrantes.reduceOption((h1,h2) => if(h1.statPrincipal > h2.statPrincipal) h1 else h2)
      if (integrantes.count(_.statPrincipal == liderPotencial.map(_.statPrincipal).get) > 1) None else liderPotencial
    }

    def venderItem(item: Item): Equipo = copy(pozoComun = pozoComun + item.valorVenta)

  }

  //==========================================================================
  // Misiones
  //==========================================================================

  type Facilidad = Equipo => Int

  abstract case class Tarea (var completada : Boolean = false) {
    def realizarPor(equipo: Equipo): Tarea = ???
    // TODO: Como implementar la facilidad de una tarea?
    // Siempre depende del equipo, pero en algunos casos también depende del heroe
    // Revisar esta firma de abajo, ya que toma la facilidad como si fuera propia del equipo, cuando puede ser del heroe que realice la tarea
    // Recordar que la facilidad puede no existir
    def getFacilidad(equipo : Equipo) : Int
    def efectoEnElHeroe(heroe : Heroe) : Heroe

  }

  // tiene una facilidad de 10 para cualquier héroe o 20 si el líder del equipo es un guerrero
  case object pelearContraMonstruo extends Tarea {
    override def efectoEnElHeroe(heroe: Heroe): Heroe = {
      if (heroe.fuerza < 20) heroe.cambiarHP(heroe.HP - 5) else heroe
    }

    // en este caso depende del equipo solamente
    override def getFacilidad(equipo: Equipo): Int = {
      if (equipo.lider.get.esGuerrero) 20 else 10
    }
  }

  case object forzarPuerta extends Tarea {
    override def efectoEnElHeroe(heroe: Heroe): Heroe = {
      ???
    }

    // en este caso depende del heroe
    override def getFacilidad(equipo: Equipo): Int = {
      ???
    }
  }

  case object robarTalisman extends Tarea {
    override def efectoEnElHeroe(heroe: Heroe): Heroe = {
      ???
    }

    // en este caso depende del heroe y del equipo
    override def getFacilidad(equipo: Equipo): Int = {
      ???
    }
  }

  case class Mision(tareas: List[Tarea], completada: Boolean = false, recompensa: Equipo => Equipo) {
    def realizarPor(equipo: Equipo): Mision = {
      val tareasSinRealizar: List[Tarea] = tareas
      tareas.foreach(tarea => tarea.realizarPor(equipo))
      if (tareas.forall(tarea => tarea.completada)) completar() else copy(tareas = tareasSinRealizar)
    }

    def completar(): Mision = {
      copy(completada = true)
    }
  }

  //==========================================================================
  // La Taberna
  //==========================================================================


}
