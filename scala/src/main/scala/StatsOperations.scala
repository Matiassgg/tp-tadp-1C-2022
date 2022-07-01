case object StatsOperations {
  type Operation = (Stat,Stat) => Stat
  val sumaDeStats : Operation = {(statBase : Stat, statModificador : Stat) => Stat(statBase.tipo, statBase.value + statModificador.value)}
  val restaDeStats : Operation = {(statBase : Stat, statModificador : Stat) => Stat(statBase.tipo, statBase.value - statModificador.value)}
}