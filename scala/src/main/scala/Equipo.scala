class Equipo(miembros: Array[Heroe]) {

  def realizarMision(mision: Mision): Boolean = {
    if (miembros
      .map(m => mision.tareasARealizar.sortWith(_.calcularFacilidad(m) < _.calcularFacilidad(m)))
      .forall( h => heroeCompletoTarea(h.zip(miembros)))) {
      obtenerRecompensa(mision.recompensa())
      return true
    }

    false
  }

  def heroeCompletoTarea(tuples: Array[(Tarea, Heroe)]): Boolean = {
    tuples.foreach {
      p => try {
        p._2.realizarTarea(p._1)
      } catch {
          case e: NoSePuedeCompletarTareaException => println(e)
        //        Dejo a los heroes como estaban
        }

        return true
    }

    false
  }

  lazy val lider = {

  }

  def agregarMiembro: Unit = {

  }

  def reemplazarMiembro: Unit = {

  }

  //  def mejorHeroe(criterio: Any): Heroe = {
  ////  heroes.
  //  }

  def obtenerItem(item: Any): Unit = {

  }

  def venderItem(item: Any): Unit = {

  }

  def obtenerRecompensa(value: Any)
}