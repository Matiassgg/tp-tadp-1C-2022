object TipoStat extends Enumeration {
  type Nombre = Value
  val HP, Fuerza, Velocidad, Inteligencia = Value
}

case class Stat(tipo : TipoStat.Nombre, _value : Int) {
  var value : Int = if (_value > 1) _value else 1
}
