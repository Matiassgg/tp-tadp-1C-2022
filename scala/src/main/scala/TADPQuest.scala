import scala.util.{Failure, Success, Try}

object TADPQuest {
  //==========================================================================
  // STATS
  //==========================================================================

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
    def cambiarHp(valor: Int) : Stats = copy(hp = sumarAtributo(hp, valor))
    def cambiarFuerza(valor: Int) : Stats = copy(fuerza = sumarAtributo(fuerza, valor))
    def cambiarInteligencia(valor: Int) : Stats = copy(inteligencia = sumarAtributo(inteligencia, valor))
    def cambiarVelocidad(valor: Int) : Stats = copy(velocidad = sumarAtributo(velocidad, valor))

    def aptoParaHeroe: Boolean = hp >= 1 && fuerza >=1 && inteligencia >= 1 && velocidad >= 1

    def normalizarParaHeroe: Stats = copy(hp = sumarAtributo(hp, 0), fuerza = sumarAtributo(fuerza, 0), inteligencia = sumarAtributo(inteligencia, 0), velocidad = sumarAtributo(velocidad, 0))
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
    override def aumentarStats: Heroe => Heroe =
      super.aumentarStats andThen(_.cambiarHp(10).cambiarFuerza(15).cambiarInteligencia(-10))
  }
  case object Mago extends Trabajo {
    def statPrincipal: Heroe => Int = _.inteligenciaBase
    override def aumentarStats: Heroe => Heroe =
      super.aumentarStats andThen(_.cambiarFuerza(-20).cambiarInteligencia(20))
  }
  case object Ladron extends Trabajo {
    def statPrincipal: Heroe => Int = _.velocidadBase
    override def aumentarStats: Heroe => Heroe =
      super.aumentarStats andThen(_.cambiarHp(-5).cambiarVelocidad(10))
  }

  //==========================================================================
  // ITEMS
  //==========================================================================

  type RestriccionItem = Heroe => Boolean

  sealed trait Item {
    def zonaEquipamiento: ZonaEquipamiento
    def restricciones: List[RestriccionItem] = List.empty
    def dosManos: Boolean = false
    def valorVenta: Int = 0
    def getStatsModificados(heroe : Heroe) : Stats
    def aplicarEfectoAHeroe(heroe: Heroe): Heroe = heroe.setStat(getStatsModificados(heroe))
  }

  case object TalismanDeDedicacion extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Cuello

    override def getStatsModificados(heroe: Heroe): Stats =
      heroe.stats + Stats().cambiarTodosLosStats(heroe.statPrincipal*0.1.toInt)
  }

  case object TalismanDelMinimalismo extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Cuello

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
    lazy val zonaEquipamiento: ZonaEquipamiento = Cuello

    override def valorVenta = 400

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

    override def getStatsModificados(heroe: Heroe): Stats =
      heroe.stats + Stats(hp = -30, velocidad = 30)
  }

  case object ArcoViejo extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Mano

    override def dosManos = true

    override def getStatsModificados(heroe: Heroe): Stats =
      heroe.stats + Stats(fuerza = 2)
  }

  case object EscudoAntiRobo extends Item {
    lazy val zonaEquipamiento: ZonaEquipamiento = Mano

    override def restricciones =
      List((h: Heroe) => !h.es(Ladron) , (h: Heroe) => h.fuerzaBase > 20)

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
  case object Cuello extends ZonaEquipamiento

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
        case Mano => if (item.dosManos) { // TODO: lo moveria a un metodo esto, y veria si hay manera de hacer PM
          copy(manos = List(Some(item)))
        } else {
          if (manos.nonEmpty && manos.head.map(_.dosManos).get) {
            copy(manos = List(Some(item)))
          } else {
            if (manos.size == 2) copy(manos = manos.tail.appended(Some(item))) else copy(manos = manos.appended(Some(item)))
          }
        }
        case Cuello => copy(talismanes = talismanes.appended(Some(item)))
      }
    }

    def items: List[Item] = List(List(cabeza, torso), manos, talismanes).flatten.flatten

    // Testear si funca el fold
    def calcularIncrementos(heroe: Heroe): Heroe =
      items.foldLeft(heroe)((buffedHero, item) => item.aplicarEfectoAHeroe(buffedHero))
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
    lazy val hpBase: Int = stats.hp
    lazy val fuerzaBase: Int = stats.fuerza
    lazy val velocidadBase: Int = stats.velocidad
    lazy val inteligenciaBase: Int = stats.inteligencia

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
    def cambiarHp(valor : Int) : Heroe = copy(stats = stats.cambiarHp(valor))
    def cambiarFuerza(valor: Int) : Heroe = copy(stats = stats.cambiarFuerza(valor))
    def cambiarInteligencia(valor: Int) : Heroe = copy(stats = stats.cambiarInteligencia(valor))
    def cambiarVelocidad(valor: Int) : Heroe = copy(stats = stats.cambiarVelocidad(valor))

    def convertirseEn(nuevoTrabajo: Trabajo): Heroe = copy(trabajo = Some(nuevoTrabajo))

    def equiparseCon(item: Item): Heroe =
      if (item.restricciones.forall( r => r(this)))
        copy (equipamiento = equipamiento.agregarItem (item) )
      else this

    def renunciar : Heroe = copy(trabajo = None)

    def setStat(stat: Stats) : Heroe = {
      val stats = if (stat.aptoParaHeroe) stat else stat.normalizarParaHeroe
      copy(stats = stats)
    }

    def es(t: Trabajo): Boolean = trabajo.contains(t)

    def esDesempleado : Boolean = trabajo.isEmpty

    // Inventario
    def cantidadItemsEquipados: Int = equipamiento.items.size

    def agregarAlInventario(item: Item): Heroe = copy(inventario = inventario.appended(item))
  }


  //==========================================================================
  // EQUIPO
  //==========================================================================

  case class Equipo(nombre: String, integrantes: Set[Heroe], pozoComun: Int = 0) {

    def mejorHeroeSegun(cuantificador: Heroe => Int): Option[Heroe] =
      integrantes.reduceOption((h1,h2) => if(cuantificador(h1) > cuantificador(h2)) h1 else h2)

    def obtenerItem(item: Item): Equipo = {
      implicit def diferenciaStatPrincipal(integrante: Heroe, item: Item) : Int =
        integrante.setStat(item.getStatsModificados(integrante)).statPrincipal - integrante.statPrincipal

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
  // TAREAS
  //==========================================================================

  type RestriccionTarea = Equipo => Boolean

  case class TareaFallidaException(tarea : Tarea) extends RuntimeException

  sealed trait Tarea {
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

    def realizarPor(equipo: Equipo): Try[Equipo] = {
      if (!puedeSerRealizadaPor(equipo)) return Failure(TareaFallidaException(this))

      val heroeSeleccionado : Option[Heroe] = equipo.heroeParaTarea(this)
      Success(provocarEfectoEn(heroeSeleccionado.get, equipo))
    }
  }

  case object pelearContraMonstruo extends Tarea {
    val reduccionDeVida : Int = 5

    override def efectoEnElHeroe(heroe: Heroe): Heroe =
      if (heroe.fuerza < 20) heroe.cambiarHp(heroe.hp - reduccionDeVida) else heroe

    override def getFacilidad(equipo: Equipo, heroe: Heroe): Option[Int] = equipo.trabajoDelLider match {
      case Some(Guerrero) => Some(20)
      case _ => Some(10)
    }
  }

  case object forzarPuerta extends Tarea {
    val aumentoDeFuerza : Int = 1
    val reduccionDehp : Int = 5

    override def efectoEnElHeroe(heroe: Heroe): Heroe =
      if (!(heroe.es(Mago) || heroe.es(Ladron))) heroe.cambiarHp(heroe.hp - reduccionDehp).cambiarFuerza(heroe.fuerza + aumentoDeFuerza) else heroe

    override def getFacilidad(equipo: Equipo, heroe: Heroe): Option[Int] =
      Some(heroe.inteligencia + 10 * equipo.integrantes.count(heroe => heroe.es(Ladron)))
  }

  case class robarTalisman(item : Item) extends Tarea {
    val talismanPorRobar : Item = item
    require(item.zonaEquipamiento.equals(Cuello),"¡Tiene que ser un talisman el item por robar!")

    override def restricciones =
      List((equipo : Equipo) => equipo.trabajoDelLider.get.equals(Ladron))

    override def efectoEnElHeroe(heroe: Heroe): Heroe =
      heroe.equiparseCon(talismanPorRobar)

    override def getFacilidad(equipo: Equipo, heroe: Heroe): Option[Int] = equipo.trabajoDelLider match {
      case Some(Ladron) => Some(heroe.velocidad)
      case _ => None
    }
  }

  //==========================================================================
  // MISIONES
  //===========================================================================

  sealed trait ResultadoMision
  case class MisionExitosa(equipo: Equipo) extends ResultadoMision
  case class MisionFallida(equipo: Equipo, tarea : Option[Tarea], razonDeFallo : Exception) extends ResultadoMision
  case class MisionFallidaException(equipo: Equipo, tareaFallida: Tarea) extends RuntimeException

  case class Mision(tareas: List[Tarea], recompensa: Equipo => Equipo) {

    def realizarPor(equipo: Equipo): ResultadoMision = {
      realizarTareas(equipo) match {
        case Success(equipoPostTareas) => MisionExitosa(recompensa(equipoPostTareas))
        case Failure(e: TareaFallidaException) => MisionFallida(equipo, Some(e.tarea), e)
        case Failure(e: Exception) => MisionFallida(equipo, None, e) // Lo agregamos por recomendación del IDE.
      }
    }

    def puedeSerRealizadaPor(equipo: Equipo): Boolean = {
      realizarPor(equipo) match {
        case MisionExitosa(_) => true
        case MisionFallida(_,_,_) => false
      }
    }

    def realizarTareas(equipo: Equipo): Try[Equipo] = tareas.foldLeft(Try(equipo))((equipoPostTarea, nuevaTarea) => nuevaTarea.realizarPor(equipoPostTarea.get))
  }

  object MisionTelequino extends Mision(
    tareas = List(forzarPuerta),
    recompensa = (equipo : Equipo) => equipo.obtenerRecompensaDeMision(500)
  )

  object MisionRobarBanco extends Mision(
    tareas = List(robarTalisman(TalismanMaldito), robarTalisman(TalismanDeDedicacion), robarTalisman(TalismanDelMinimalismo)),
    recompensa = (equipo : Equipo) => equipo.obtenerRecompensaDeMision(12000)
  )

  object MisionNuevoCamarada extends Mision(
    tareas = List(pelearContraMonstruo),
    recompensa = (equipo : Equipo) => equipo.obtenerMiembro(
      heroe = Heroe(Stats(50, 1, 25, 10), List.empty, Equipamiento(None, None, List.empty, List.empty), None)
    )
  )

  //==========================================================================
  // LA TABERNA
  //==========================================================================

  case class EleccionFallida(message: String) extends RuntimeException
  type Criterio = (Equipo, Equipo) => Boolean

  case class Taberna(misiones: Set[Mision]) {

    def obtenerMejorMision(equipo: Equipo, criterio: Criterio, misiones: Set[Mision]): Try[Mision] = {
      val mejorMisionPotencial = misiones.reduce((mision1, mision2) => {
        mision1.realizarPor(equipo) match {
          case MisionExitosa(equipo1) => mision2.realizarPor(equipo) match {
            case MisionExitosa(equipo2) => if (criterio(equipo1, equipo2)) mision1 else mision2
            case MisionFallida(_, _, _) => mision1
          }
          case MisionFallida(_, _, _) => mision2
        }
      })
      if (mejorMisionPotencial.puedeSerRealizadaPor(equipo))
        Success(mejorMisionPotencial)
      else  Failure(EleccionFallida("No hay mision para elegir"))
    }

    def elegirMision(equipo: Equipo, criterio: Criterio, misiones: Set[Mision]): Option[Mision] = {
      obtenerMejorMision(equipo, criterio, misiones) match {
        case Success(mejorMision) => Some(mejorMision)
        case Failure(_) => None
      }
    }

    def entrenar(equipo: Equipo, criterio: Criterio, misiones : Set[Mision]) : Equipo = {
      val mision : Mision = elegirMision(equipo, criterio, misiones).get
      val nuevasMisiones = misiones - mision

      mision.realizarPor(equipo) match {
        case MisionExitosa(equipoPostMision) => if(nuevasMisiones.isEmpty) equipo else entrenar(equipoPostMision, criterio, nuevasMisiones)
        case MisionFallida(_,_,_) => equipo
      }
    }
  }
}
