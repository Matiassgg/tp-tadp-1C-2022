trait TipoItem
object Cabeza extends TipoItem
object Torso extends TipoItem
object Mano extends TipoItem
object Talisman extends TipoItem

class Item(nombreItem: String, val tipoItem: TipoItem, val statsAfectados: List[Stat])
