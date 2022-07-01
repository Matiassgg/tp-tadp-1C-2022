object RestrictionOperation
sealed trait RestrictionOperation {
  def apply(heroe: Heroe) : Boolean
}

object HeroStatComparator
case class HeroStatComparator(heroe: Heroe, tipoStatNombre : TipoStat.Nombre, valorMinimoDeStat : Int) extends RestrictionOperation {
  def apply(heroe: Heroe) : Boolean = heroe.getValueOfStat(tipoStatNombre) >= valorMinimoDeStat
}

object BetweenStatsComparator
case class BetweenStatsComparator(heroe : Heroe, tipoStatMejor : TipoStat.Nombre, tipoStatPeor : TipoStat.Nombre) extends RestrictionOperation {
  def apply(heroe: Heroe) : Boolean = heroe.getValueOfStat(tipoStatMejor) >= heroe.getValueOfStat(tipoStatPeor)
}

// TODO: Revisar para comparar si tiene *o no* el trabajo
object WorkComparator
case class WorkComparator(heroe : Heroe, especialidad : Trabajo) extends RestrictionOperation {
  def apply(heroe: Heroe) : Boolean = heroe.getTrabajo.equals(especialidad)
}
