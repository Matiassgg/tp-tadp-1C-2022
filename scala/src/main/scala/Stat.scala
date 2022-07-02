case class Stats(
    HP: HP,
    inteligencia: Inteligencia,
    velocidad: Velocidad,
    fuerza: Fuerza
) {
}

sealed trait Stat
case class HP(value: Int) extends Stat{
  require(value > 1, "HP debe ser mayor a 1")
}
case class Inteligencia(value: Int) extends Stat{
  require(value > 1, "inteligencia debe ser mayor a 1")
}
case class Velocidad(value: Int) extends Stat{
  require(value > 1, "fuerza debe ser mayor a 1")
}

case class Fuerza(value: Int) extends Stat{
  require(value > 1, "velocidad debe ser mayor a 1")
}
