import TipoItem.Tipo

import scala.collection.mutable

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

abstract class Equipo(heroes: Array[Heroe]) {

  lazy val lider = {

  }

  def agregarMiembro: Unit

  def reemplazarMiembro: Unit

  //  def mejorHeroe(criterio: Any): Heroe = {
  ////  heroes.
  //  }

  def facilidadDeMision(mision: Mision): Int

  def obtenerItem(item: Any): Unit

  def venderItem(item: Any): Unit
}

abstract class Mision(tareas: Array[Tarea]) {

  def informarEstadoDeMision: Unit

  def recompensarEquipoGanador: Unit // [Coleccionable]

  def pudoRealizarMision(equipo: Equipo): Boolean = {
    //    tareas.asignarHeroe()
    true
    //    if (!tareas.isEmpty)
  }

  //  def puedeSerCompletada(equipo: Equipo): Boolean = {
  //
  //  }
}

class Tarea {

}

class Restriccion {
  def cumpleRestriccion(heroe: Heroe): Boolean = {
    true
  }
}

class Heroe {
  val stats = new Stats().initStats
  val inventario = new Inventario(this).initInventario
  var trabajo: Option[Trabajo] = _

  def realizarTarea(tarea: Tarea): Unit = {

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
}

class Trabajo {

}

class Taberna(misiones: Array[Mision], equipo: Equipo) {

  def entrenar(misionARealizar: Option[Mision] = misiones.headOption) {
    if (misionARealizar.get.pudoRealizarMision(equipo))
      entrenar(seleccionarMision())
  }

  def seleccionarMision(): Option[Mision] = {
    //    TODO WIP
    Option(misiones(1))
  }

  def elegirMision(mision1: Mision, mision2: Mision): Boolean = {
    //    TODO WIP
    //    {(e1, e2) => e1.oro > e2.oro}
    return true
  }
}