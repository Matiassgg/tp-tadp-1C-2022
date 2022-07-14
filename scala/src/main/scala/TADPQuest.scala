object TADPQuest {
  //==========================================================================
  // STATS
  //==========================================================================

  case class Incrementos (hp: Int = 0, inteligencia: Int = 0, fuerza: Int = 0, velocidad: Int = 0)

  case class Stats (hp: Int = 0, inteligencia: Int= 0, fuerza: Int= 0, velocidad: Int = 0) {
    def +(stats: Stats): Stats = copy(
      fuerza       = fuerza + stats.fuerza,
      hp           = hp + stats.hp,
      inteligencia = inteligencia + stats.inteligencia,
      velocidad    = velocidad + stats.velocidad,
    )

    def -(stats: Stats): Stats = copy(
      fuerza       = fuerza - stats.fuerza,
      hp           = hp - stats.hp,
      inteligencia = inteligencia - stats.inteligencia,
      velocidad    = velocidad - stats.velocidad,
    )

    def cambiarTodosLosStats(valor: Int): Stats = copy(
      fuerza       = valor,
      hp           = valor,
      inteligencia = valor,
      velocidad    = valor,
    )

    def sumarAtributo(atributo1: Int, atributo2: Int) : Int = (atributo1+atributo2).max(1)
    def cambiarhp(valor: Int) : Stats = copy(hp = sumarAtributo(hp, valor))
    def cambiarFuerza(valor: Int) : Stats = copy(fuerza = sumarAtributo(fuerza, valor))
    def cambiarInteligencia(valor: Int) : Stats = copy(inteligencia = sumarAtributo(inteligencia, valor))
    def cambiarVelocidad(valor: Int) : Stats = copy(velocidad = sumarAtributo(velocidad, valor))

    def recalcularStats(incrementos: Incrementos): Stats = cambiarhp(incrementos.hp).cambiarFuerza(incrementos.fuerza).cambiarVelocidad(incrementos.velocidad).cambiarInteligencia(incrementos.inteligencia)
  }

  //==========================================================================
  // TRABAJO
  //==========================================================================

  trait Trabajo {
    def statPrincipal: Heroe => Int
    def aumentarStats: Heroe => Heroe = (h: Heroe) => h.setStat(h.stats)
  }

  case object Guerrero extends Trabajo {
    def statPrincipal: Heroe => Int = _.fuerzaBase
    override def aumentarStats: Heroe => Heroe = {
      super.aumentarStats andThen(_.cambiarhp(10).cambiarFuerza(15).cambiarInteligencia(-10))
    }
  }
  case object Mago extends Trabajo {
    def statPrincipal: Heroe => Int = _.inteligenciaBase
    override def aumentarStats: Heroe => Heroe = {
      super.aumentarStats andThen(_.cambiarFuerza(-20).cambiarInteligencia(20))
    }
  }
  case object Ladron extends Trabajo {
    def statPrincipal: Heroe => Int = _.velocidadBase
    override def aumentarStats: Heroe => Heroe = {
      super.aumentarStats andThen(_.cambiarhp(-5).cambiarVelocidad(10))
    }
  }

  //==========================================================================
  // ITEMS
  //==========================================================================

  type RestriccionItem = Heroe => Boolean

  // TODO: tal vez es el momento para borrar esto...
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

  sealed trait Item {
    def zonaEquipamiento: ZonaEquipamiento
    def restricciones: List[RestriccionItem] = List.empty
    def dosManos: Boolean = false
    def valorVenta: Int = 0
    def getStatsModificados(heroe : Heroe) : Stats
    def aplicarEfectoAHeroe(heroe: Heroe): Heroe = heroe.setStat(getStatsModificados(heroe))
  }

  case object TalismanDeDedicacion extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Talisman

    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats + Stats().cambiarTodosLosStats(heroe.statPrincipal*0.1.toInt)
    }
  }

  case object TalismanDelMinimalismo extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Talisman

    override def getStatsModificados(heroe: Heroe): Stats =
      heroe.stats + Stats(hp= 50-(10*heroe.cantidadItemsEquipados))
  }

  case object VinchaDelBufaloDeAgua extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Cabeza
    override def restricciones = List( (h: Heroe) => h.esDesempleado )

    override def getStatsModificados(heroe: Heroe): Stats = {
      if(heroe.fuerzaBase > heroe.inteligenciaBase) heroe.stats + Stats(inteligencia = 30)
      else heroe.stats + Stats(10,0,10,10)
    }
  }

  case object TalismanMaldito extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Talisman

    override def getStatsModificados(heroe: Heroe): Stats = Stats(1,1,1,1)
  }

  case object EspadaDeLaVida extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento= Mano

    override def getStatsModificados(heroe: Heroe): Stats = 
      heroe.stats + Stats(fuerza = (heroe.fuerzaBase * -1) + heroe.hpBase)
  }

  case object CascoVikingo extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Cabeza
    override def restricciones = List( (h: Heroe) => h.fuerzaBase > 30)

    override def getStatsModificados(heroe: Heroe): Stats = heroe.stats + Stats(hp = 10)
  }

  case object PalitoMagico extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Mano

    override def restricciones = List((h: Heroe) => h.es(Mago) || (h.es(Ladron) && h.inteligenciaBase > 30))

    override def getStatsModificados(heroe: Heroe): Stats = heroe.stats + Stats(inteligencia = 20)
  }

  case object ArmaduraEleganteSport extends Item{
    lazy val zonaEquipamiento: ZonaEquipamiento = Torso

    override def valorVenta = 400

    override def getStatsModificados(heroe: Heroe): Stats =
      heroe.stats.recalcularStats(Incrementos(-30,0,0,30))
  }

  case object ArcoViejo extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Mano

    override def dosManos = true

    override def getStatsModificados(heroe: Heroe): Stats =
      heroe.stats + Stats(fuerza = 2)
  }

  case object EscudoAntiRobo extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Mano

    override def restricciones = List((h: Heroe) => !h.es(Ladron) , (h: Heroe) => h.fuerzaBase > 20)

    override def getStatsModificados(heroe: Heroe): Stats = {
      heroe.stats + Stats(hp = 20)
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
                    cabeza: Option[Item],
                    torso: Option[Item],
                    manos: List[Option[Item]],
                    talismanes: List[Option[Item]]
                    ){
    require(manos.size <= 2, "Solo hay dos manos!")

    def agregarItem(item: Item) : Equipamiento = {
      item.zonaEquipamiento match {
        case Cabeza => copy(cabeza = Some(item))
        case Torso => copy(torso = Some(item))
        case Mano => if (item.dosManos) {
          copy(manos = List(Some(item)))
        } else {
          if (manos.nonEmpty && manos.head.map(_.dosManos).get) {
            copy(manos = List(Some(item)))
          } else {
            if (manos.size == 2) copy(manos = manos.tail.appended(Some(item))) else copy(manos = manos.appended(Some(item)))
          }
        }
        case Talisman => copy(talismanes = talismanes.appended(Some(item)))
      }
    }

    def items: List[Item] = List(List(cabeza, torso), manos, talismanes).flatten.flatten

    // Testear si funca el fold
    def calcularIncrementos(heroe: Heroe): Heroe = items.foldLeft(heroe)((buffedHero, item) => item.aplicarEfectoAHeroe(buffedHero))
  }

  //==========================================================================
  // HEROE
  //==========================================================================

  case class Heroe(stats: Stats, inventario: List[Item], equipamiento: Equipamiento, trabajo : Option[Trabajo]) {
    require(hpBase >= 1, "hp debe ser mayor a 0")
    require(fuerzaBase >= 1, "inteligencia debe ser mayor a 0")
    require(velocidadBase >= 1, "fuerza debe ser mayor a 0")
    require(inteligenciaBase >= 1, "velocidad debe ser mayor a 0")

    lazy val statPrincipal: Int = trabajo.map(_.statPrincipal(this)).getOrElse(0)
    
    // Stats base
    lazy val hpBase = stats.hp
    lazy val fuerzaBase = stats.fuerza
    lazy val velocidadBase = stats.velocidad
    lazy val inteligenciaBase = stats.inteligencia

    def statsConIncrementos: Stats = {
      val heroeConIncrementos = trabajo match {
        case Some(unTrabajo) => unTrabajo.aumentarStats(this)
        case None => this
      }

      equipamiento.calcularIncrementos(heroeConIncrementos).stats
    }

    // Stats con buffs
    def hp : Int = statsConIncrementos.hp
    def velocidad : Int = statsConIncrementos.velocidad
    def inteligencia : Int = statsConIncrementos.inteligencia
    def fuerza : Int = statsConIncrementos.fuerza

    // Stats Setters
    def cambiarhp(valor : Int) : Heroe = copy(stats = stats.cambiarhp(valor))
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

    //https://www.scala-lang.org/api/2.12.1/scala/Option.html#contains[A1%3E:A](elem:A1):Boolean
    def es(t: Trabajo): Boolean = trabajo.contains(t)

    def esDesempleado : Boolean = trabajo.isEmpty

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
      implicit def diferenciaStatPrincipal(integrante: Heroe, item: Item) : Int = integrante.equiparseCon(item).statPrincipal - integrante.statPrincipal
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

    lazy val trabajoDelLider: Option[Trabajo] = for {
      lider <- lider
      trabajo <- lider.trabajo
    } yield trabajo

    private def agregarOro(valorPorAgregar : Int): Equipo = copy(pozoComun = pozoComun + valorPorAgregar)

    def venderItem(item: Item): Equipo = agregarOro(item.valorVenta)

    def obtenerRecompensaDeMision(recompensa : Int) : Equipo = agregarOro(recompensa)

    def heroeParaTarea(tarea: Tarea): Option[Heroe] = Some(integrantes.maxBy(heroe => tarea.getFacilidad(this, heroe)))
  }

  //==========================================================================
  // Misiones
  //==========================================================================

  type RestriccionTarea = Equipo => Boolean

  sealed trait Tarea {
    var completada: Boolean = false
    def restricciones: List[RestriccionTarea] = List.empty
    def getFacilidad(equipo: Equipo, heroe: Heroe): Option[Int]
    def efectoEnElHeroe(heroe: Heroe): Heroe

    def puedeSerRealizadaPor(equipo: Equipo) : Boolean = {
      restricciones.forall(restriccion => restriccion(equipo))
    }

    def provocarEfectoEn(heroeSeleccionado: Heroe, equipo: Equipo) : Equipo = {
      val heroeSeleccionadoPostTarea : Heroe = efectoEnElHeroe(heroeSeleccionado)
      equipo.reemplazarMiembro(heroeSeleccionado, heroeSeleccionadoPostTarea)
    }

    def realizarPor(equipo: Equipo): Tarea = {
      if (!puedeSerRealizadaPor(equipo)) return this

      // TODO: retornar la tarea completada una vez provocados los efectos sobre el heroe ??
      // retorno tarea
      // val heroeSeleccionado : Option[Heroe] = equipo.heroeParaTarea(this)
      // provocarEfectoEn(heroeSeleccionado.get, equipo)
      // copy(completada = true) // rompe


      // TODO: o retornar el equipo una vez completada la tarea ??
      // retorno equipo
      // val heroeSeleccionado : Option[Heroe] = equipo.heroeParaTarea(this)
      // provocarEfectoEn(heroeSeleccionado.get, equipo)

      this

    }
  }

  case object pelearContraMonstruo extends Tarea {
    val reduccionDeVida : Int = 5

    override def efectoEnElHeroe(heroe: Heroe): Heroe = {
      if (heroe.fuerza < 20) heroe.cambiarhp(heroe.hp - reduccionDeVida) else heroe
    }

    override def getFacilidad(equipo: Equipo, heroe: Heroe): Option[Int] = equipo.trabajoDelLider match {
      case Some(Guerrero) => Some(20)
      case _ => Some(10)
    }
  }

  case object forzarPuerta extends Tarea {
    val aumentoDeFuerza : Int = 1
    val reduccionDehp : Int = 5

    override def efectoEnElHeroe(heroe: Heroe): Heroe = {
      if (!(heroe.es(Mago) || heroe.es(Ladron))) heroe.cambiarhp(heroe.hp - reduccionDehp).cambiarFuerza(heroe.fuerza + aumentoDeFuerza) else heroe
    }

    override def getFacilidad(equipo: Equipo, heroe: Heroe): Option[Int] =
      Some(heroe.inteligencia + 10 * equipo.integrantes.count(heroe => heroe.es(Ladron)))
  }

  case object robarTalisman extends Tarea {
    override def restricciones = List((equipo : Equipo) => equipo.trabajoDelLider.get.equals(Ladron))

    override def efectoEnElHeroe(heroe: Heroe): Heroe = {
      // TODO: Cómo hacer para que este talisman sea random o definido por la tarea ??
      heroe.equiparseCon(TalismanDeDedicacion)
    }

    override def getFacilidad(equipo: Equipo, heroe: Heroe): Option[Int] = equipo.trabajoDelLider match {
      case Some(Ladron) => Some(heroe.velocidad)
      case _ => None
    }
  }

  case class Mision(tareas: List[Tarea], completada: Boolean = false, recompensa: Equipo => Equipo) {
    def realizarPor(equipo: Equipo): Mision = {
      val tareasSinRealizar: List[Tarea] = tareas
      // TODO : Revisar como "enterarse" si las tareas fueron completadas, esta modelado con efecto ...
      tareas.foreach(tarea => tarea.realizarPor(equipo))
      if (tareas.forall(tarea => tarea.completada)) completar() else copy(tareas = tareasSinRealizar)
    }

    def otorgarRecompensaPara(equipo: Equipo) : Equipo = {
      if (completada) recompensa(equipo) else equipo
    }

    private def completar(): Mision = {
      copy(completada = true)
    }

  }

  // Por tirar nombres ...

  object MisionTelequino extends Mision(
    tareas = List(forzarPuerta),
    recompensa = (equipo : Equipo) => equipo.obtenerRecompensaDeMision(500)
  )

  object MisionNuevoCamarada extends Mision(
    tareas = List(pelearContraMonstruo),
    recompensa = (equipo : Equipo) => equipo.obtenerMiembro(
      heroe = Heroe(Stats(50, 1, 25, 10), List.empty, Equipamiento(None, None, List.empty, List.empty), None)
    )
  )

  //==========================================================================
  // La Taberna
  //==========================================================================


}
