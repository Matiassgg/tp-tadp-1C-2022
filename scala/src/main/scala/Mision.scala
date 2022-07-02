class Mision(tareas: List[Tarea]) {

  def tareasARealizar = tareas

  def verTareas() = tareas.foreach(t => t.getNombre)

  def pudoRealizarMision(equipo: Equipo): Boolean = equipo.realizarMision(this)
}

class Tarea(nombre: String, consecuencia: List[Efecto], restricciones: List[Restriccion]) {

  def cumpleRequisitos(heroe: Heroe): Boolean =
    restricciones.forall(r => heroe.tieneRequisito(r))

  def fueCompletada(heroe: Heroe): Boolean = {
    if (cumpleRequisitos(heroe)) {
      heroe.sufrirEfectosDeRealizarTarea(consecuencia)
      return true
    }

    false
  }

  def getNombre = nombre

  def calcularFacilidad(heroe: Heroe): Int = {
    5
  }
}