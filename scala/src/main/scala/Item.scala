trait TipoItem
object Cabeza extends TipoItem
object Torso extends TipoItem
object Mano extends TipoItem
object Talisman extends TipoItem

case class ItemInvalidoException(message: String) extends RuntimeException(message)

sealed trait ResultadoChequeoDeItems
case class ItemValidoPara(Heroe: Heroe) extends ResultadoChequeoDeItems
case class ItemInvalidoPara(Heroe: Heroe, razon: Exception) extends ResultadoChequeoDeItems

class Item(nombreItem: String, val tipoItem: TipoItem, val statsAfectados: List[Stat], val restricciones : List[Restriccion]) {

  def validarRestriccionesCon(heroe : Heroe): ResultadoChequeoDeItems = {
    try {
      if (restricciones.forall(restriccion => restriccion.verificar(heroe))) {
        ItemValidoPara(heroe)
      } else {
        ItemInvalidoPara(heroe, ItemInvalidoException("El heroe no contiene el item en su inventario"))
      }
    } catch {
      case e: ItemInvalidoException => ItemInvalidoPara(heroe, e)
    }
  }

}
