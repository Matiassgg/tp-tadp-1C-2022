import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
    println("1- Elegir mision \n2- Entrenar ")

    scala.io.StdIn.readInt() match {
      case 1 => seleccionarMision()
      case 2 => Pruebas.taberna.entrenar()
    }
  }

  def seleccionarMision(): Unit = {
    println("Elija la primera mision a comparar")
    Pruebas.taberna.verMisionesDisponibles()
    val mIndex1 = scala.io.StdIn.readInt()
    println("Elija la segunda mision a comparar")
    Pruebas.taberna.verMisionesDisponibles()
    val mIndex2 = scala.io.StdIn.readInt()

    Pruebas.taberna.elegirMision(mIndex1, mIndex2, true)
  }
}

sealed class Taberna(misiones: ListBuffer[Mision], equipo: Equipo) {

  def entrenar(misionARealizar: Option[Mision] = misiones.headOption) {
    if (misionARealizar.get.pudoRealizarMision(equipo)) {
      misiones -= misionARealizar.getOrElse { return }
      entrenar(Option(proximaMision()))
    }
  }

  def proximaMision(): Mision = {
    println("Elegir proxima mision")
    try {
      verMisionesDisponibles()
    } catch e {
      println(e)
    }
    val mIndex1 = scala.io.StdIn.readInt()
    misiones.apply(mIndex1)
//    println("Elegir otra mision")
//    verMisionesDisponibles()
//    val mIndex2 = scala.io.StdIn.readInt()
//    val m2 = misiones.apply(mIndex1)

//    val misionElegida = elegirMision(m1, m2) match {
//      case
//    }
  }

  def verMisionesDisponibles() = misiones.zipWithIndex.foreach(tuple => println(s"${tuple._2}-${tuple._1.verTareas()}"))

  def elegirMision(mision1: Int, mision2: Int, condicion: Boolean): Boolean = {
    //    TODO WIP
    //    {(e1, e2) => e1.oro > e2.oro}
    true
  }
}

// TODO: Borrar
object Pruebas {

//  Tareas
  val efectoPelea = new Efecto(Stats.decrementar(5), TipoStat.HP)
  var peleaMounstro = new Tarea("pelear contra mounstro", Array(efectoPelea), Array())
  val mision1 = new Mision(Array(peleaMounstro, peleaMounstro))

  val taberna = new Taberna(ListBuffer(mision1), new Equipo(Array(new Heroe(), new Heroe(), new Heroe())))
}

class Efecto(var operacion: Int => Int, var stat: TipoStat.Nombre)
