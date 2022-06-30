case class Heroe(stats : List[Stat], inventario : List[Item], itemsEquipados : List[Item] , trabajo : Trabajo) {
// No se si trabajo debería ser option
  def modificarStat(statTipo: TipoStat.Nombre, valueStat: Int, operacionSobreStat: StatsOperations.Operation): List[Stat] = {
    this.stats match {
      case statsHead :: statBase :: tailStats => {
        if (statBase.tipo == statTipo) statsHead :: operacionSobreStat(statBase, Stat(statTipo, valueStat)) :: tailStats else this.stats
      }
    }
  }

  // Debería ademas de cambiar de trabajo, aplicar los efectos del mismo.
  def cambiarDeTrabajo(trabajoNuevo: Trabajo): Heroe = {
    copy(trabajo= trabajoNuevo)
  }
}