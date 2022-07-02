object TipoItem extends Enumeration {
  type Tipo = Value

  val Cabeza, Torso, Mano, Talisman = Value
}

class Item(nombre: String, val tipo: TipoItem.Tipo, val restricciones: Array[Restriccion], val powerUps: Array[Stat])
//var casco = new Item("casco vikingo", TipoItem.Cabeza, Array(), Array(new Stat(TipoStat.HP, 10)))