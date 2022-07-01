import TipoStat.HP
import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.immutable.Map

class ProjectSpec extends AnyFreeSpec {

  "TADPQuest Tests" - {

    "Tests de stats" - {
      "Los stats creados nunca pueden tener valores negativos" in {
        var statHP = Stat(TipoStat.HP, 10)
        statHP.value shouldBe 10
        statHP = Stat(TipoStat.HP, -1)
        statHP.value shouldBe 1
      }
    }

    "Tests de Heroe" - {
      "Tests de Trabajo" - {
        "El heroe puede cambiar de trabajo" in {
          val statsAfectados = List(
            Map(Stat(TipoStat.HP, 10) -> StatsOperations.sumaDeStats),
            Map(Stat(TipoStat.Fuerza, 15) -> StatsOperations.sumaDeStats),
            Map(Stat(TipoStat.Inteligencia, 10) -> StatsOperations.restaDeStats)
          )
          val guerrero = new Trabajo(TipoStat.Fuerza, statsAfectados)
          val ladron = new Trabajo(TipoStat.Fuerza, statsAfectados)
          val heroeBase = new Heroe(null, null, null, guerrero)

          val heroeConvertido: Heroe = heroeBase.convertirseEn(ladron)
          heroeConvertido.getTrabajo shouldBe ladron
        }
      }

      "Tests de Item" - {
        "Los items de un heroe afectan a sus stats" in {
          val statsBaseHeroe: List[Stat] = List[Stat](Stat(TipoStat.HP, 10), Stat(TipoStat.Fuerza, 10), Stat(TipoStat.Velocidad, 10), Stat(TipoStat.Inteligencia, 10))
          val itemsIniciales: List[Item] = List.empty[Item]
          val inventarioInicial: List[Item] = List.empty[Item]
          val heroeBase = new Heroe(statsBaseHeroe, itemsIniciales, inventarioInicial, null)

          heroeBase.modificarStat(TipoStat.HP, 50, StatsOperations.sumaDeStats)

          heroeBase.getValueOfStat(HP) shouldBe 50
        }
      }
    }
  }
}