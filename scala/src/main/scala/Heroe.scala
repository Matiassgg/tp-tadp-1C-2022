import TipoItem.Tipo

import scala.collection.mutable

class NoSePuedeCompletarTareaException(s: String) extends Exception(s)

class Heroe {
  val stats = new Stats().initStats
  val inventario = new Inventario(this).initInventario
  var trabajo: Option[Trabajo] = _

  @throws(classOf[NoSePuedeCompletarTareaException])
  def realizarTarea(tarea: Tarea): Unit = {
    if (!tarea.fueCompletada(this))
      throw new NoSePuedeCompletarTareaException(s"La tarea ${tarea.getNombre} no pudo ser completada. Mision fallida")
  }

  def sufrirEfectosDeRealizarTarea(efectos: Array[Efecto]): Unit = {
    efectos.foreach( e => stats.modificar(new Stat(e.stat, stats(e.stat)), e.operacion))
  }

  def leerStats: Unit = {
    stats.foreach(x => println(s"nombre: ${x._1} valor: ${x._2}"))
  }

  def equipar(item: Item): Unit = {
    val puedeEquipar = item.restricciones.forall(r => r.cumpleRestriccion(this))
    if (puedeEquipar) inventario.agregarAInventario(item) else println("El item no puede ser equipado")
  }

  def guardarEnInventario(item: Item): Unit = {

    //    Se equipa siempre y cuando cumpla las restricciones
  }

  //  def obtenerDeInventario: Item = {
  //    Leo el inventario con un numero para cada item
  //    usuario ingresa numero
  //    se devuelve ese item
  //  }

  def convertirseEn(nuevoTrabajo: Trabajo): Unit = {
    nuevoTrabajo.powerUps.foreach(p => stats.modificar(p, Stats.incrementar(p.valor)))
    nuevoTrabajo.quedarEfectivo(this)
    trabajo = Option(nuevoTrabajo)
  }

  def renunciar(actualTrabajo: Trabajo): Unit = {
    actualTrabajo.powerUps.foreach(p => stats.modificar(p, Stats.decrementar(p.valor)))
  }

  def cambiarTrabajo(nuevo: Trabajo): Unit = {
    if (trabajo.isDefined) {
      renunciar(trabajo.get)
    }

    convertirseEn(nuevo)
  }

  def tieneRequisito(r: Restriccion): Boolean = {
true
  }
}

class Inventario(heroe: Heroe) extends mutable.HashMap[TipoItem.Tipo, Array[Item]] {

  def initInventario: Inventario = {
    TipoItem.values.foreach(i => this (i) = Array())
    this
  }

  override def addOne(elem: (Tipo, Array[Item])): Inventario.this.type = {
    if (this (elem._1).isEmpty) {
      this (elem._1) = elem._2
    } else {
      this (elem._1).foreach {
        i =>
          desequipar(i)
          this.update(elem._1, elem._2)
      }
    }

    equipar(this (elem._1).head)
    this
  }

  def desequipar(item: Item): Unit = {
    item.powerUps.foreach(p => heroe.stats.modificar(p, Stats.decrementar(p.valor)))
  }

  def equipar(item: Item): Unit = {
    item.powerUps.foreach(p => heroe.stats.modificar(p, Stats.incrementar(p.valor)))
  }

  def agregarAInventario(item: Item): Unit = item.tipo match {
    case TipoItem.Talisman => this (item.tipo) = Array(item)
    case _ => this.addOne((item.tipo, Array(item)))
  }
}

class Trabajo(nombre: String, val statPrincipal: TipoStat.Nombre, val powerUps: Array[Stat]) {

  def quedarEfectivo(heroe: Heroe): Unit = {
    //    heroe.
  }
}

class Restriccion {
  def cumpleRestriccion(heroe: Heroe): Boolean = {
    true
  }
}