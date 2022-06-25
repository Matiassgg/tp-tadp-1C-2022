case class ItemInvalidoException(message: String) extends RuntimeException(message)

class Heroe(stats : List[Stat], inventario : List[Item], itemsEquipados : List[Item], trabajo : Trabajo) {

  def modificarStat(statTipo: TipoStat.Nombre, valueStat : Int, operacionSobreStat : StatsOperations.Operation): List[Stat] = {
    this.stats match {
      case statsHead :: statBase :: tailStats => {
        if (statBase.tipo == statTipo) statsHead :: operacionSobreStat(statBase, Stat(statTipo, valueStat)) :: tailStats else this.stats
      }
    }
  }

  def equiparItem(item : Item): List[Item] = {
    if (!inventario.contains(item)) throw ItemInvalidoException("El item debe estar en el inventario para equiparlo!")
    // TODO: Chequear restricciones del item para el heroe antes de tratar de agregarlo
    // item.validarRestriccionesCon(this)
    this.itemsEquipados match {
      case itemsHead :: itemMismoTipo :: itemsTail =>
        if (item.tipoItem == itemMismoTipo.tipoItem) itemsHead :: item :: itemsTail else this.itemsEquipados.appended(item)
    }
  }

  def convertirseEn(nuevoTrabajo: Trabajo): Heroe = {
    new Heroe(this.stats, this.inventario, this.itemsEquipados, nuevoTrabajo)
  }

  def getValueOfStat(tipoStat : TipoStat.Nombre) = {
    // TODO: Terminar de revisar
//    val mapStatTrabajo = trabajo.statsAfectados.find(statMap => statMap.keys)
//    val statBaseHeroe = stats.find(stat => stat.tipo == tipoStat)
//    val statsItemsEquipados = itemsEquipados.filter(item => item.statsAfectados.contains(tipoStat))
  }
}
