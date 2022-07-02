case class Item(cuerpoHeroe: CuerpoHeroe, incrementos: Incrementos, restriccion: Heroe => Boolean) {

}

sealed trait CuerpoHeroe
case object Cabeza extends CuerpoHeroe
case object Torso extends CuerpoHeroe
case object Mano extends CuerpoHeroe
case object Talisman extends CuerpoHeroe



