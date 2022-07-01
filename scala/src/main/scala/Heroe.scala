sealed trait ResultadoItemEquipado
case class ItemEquipadoExitoso(Heroe: Heroe) extends ResultadoItemEquipado
case class ItemEquipadoFallido(Heroe: Heroe, razon: Exception) extends ResultadoItemEquipado

class Heroe(stats : List[Stat], inventario : List[Item], itemsEquipados : List[Item], trabajo : Trabajo) {

  def modificarStat(statTipo: TipoStat.Nombre, valueStat : Int, operacionSobreStat : StatsOperations.Operation): List[Stat] = {
    this.stats match {
      case statsHead :: statBase :: tailStats =>
        if (statBase.tipo == statTipo) statsHead :: operacionSobreStat(statBase, Stat(statTipo, valueStat)) :: tailStats else this.stats

    }
  }

  def equiparItem(item : Item): ResultadoItemEquipado = {
    try {
      item.validarRestriccionesCon(this)
      this.itemsEquipados match {
        case List() => ItemEquipadoExitoso(new Heroe(this.stats,this.inventario,this.itemsEquipados.appended(item),this.trabajo))
        case _ :: itemMismoTipo :: _ =>
          if (item.tipoItem == itemMismoTipo.tipoItem) {
            ItemEquipadoExitoso(new Heroe(this.stats,this.inventario,this.itemsEquipados.appended(item),this.trabajo))
          } else {
            ItemEquipadoFallido(this, ItemInvalidoException("No se pudo equipar el item!"))
          }
      }
    } catch {
      case e: ItemInvalidoException => ItemEquipadoFallido(this, e)
    }
  }

  def convertirseEn(nuevoTrabajo: Trabajo): Heroe = {
    new Heroe(this.stats, this.inventario, this.itemsEquipados, nuevoTrabajo)
  }

  def getTrabajo: Trabajo = trabajo


  def getValueOfStat(tipoStat : TipoStat.Nombre): Int = {
    // TODO: Terminar de revisar
//    val mapStatTrabajo = trabajo.statsAfectados.find(statMap => statMap.keys)
//    val statBaseHeroe = stats.find(stat => stat.tipo == tipoStat)
//    val statsItemsEquipados = itemsEquipados.filter(item => item.statsAfectados.contains(tipoStat))
    1
  }
}
