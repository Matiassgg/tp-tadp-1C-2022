import scala.collection.mutable

object TipoStat extends Enumeration {
  type Nombre = Value

  val HP, Fuerza, Velocidad, Inteligencia = Value
}

class Stat(val tipoStat: TipoStat.Nombre, val valor: Int)

object Stats {
  def incrementar(num1: Int)(num2: Int): Int = num1 + num2

  def decrementar(num1: Int)(num2: Int): Int = num1 - num2
}

class Stats extends mutable.HashMap[TipoStat.Nombre, Int] {

  def initStats =
    this.addAll(TipoStat.values.map(x => (x, 0)).toMap)

  def modificar(stat: Stat, op: Int => Int): Unit = {
    this (stat.tipoStat) = op(stat.valor)
  }
}