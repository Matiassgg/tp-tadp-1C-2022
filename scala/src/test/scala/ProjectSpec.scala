import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec
import  scala.collection.immutable.Map

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


      "Los items de un heroe afectan a sus stats" in {
        val statsBaseHeroe : List[Stat] = List[Stat](Stat(TipoStat.HP, 10), Stat(TipoStat.Fuerza, 10), Stat(TipoStat.Velocidad, 10), Stat(TipoStat.Inteligencia, 10))
        val itemsIniciales : List[Item] = List.empty[Item]
        val inventarioInicial : List[Item] = List.empty[Item]
        val heroeBase = new Heroe(statsBaseHeroe, itemsIniciales, inventarioInicial,null)

        // TODO: HP del heroe deberia ser 10

        heroeBase.modificarStat(TipoStat.HP,50, StatsOperations.sumaDeStats)

        // TODO: HP del heroe deberia ser 60

        1 shouldBe 1
      }

      "El heroe puede cambiar de trabajo" in {
        val statsAfectados = List(
          Map(Stat(TipoStat.HP,10) -> StatsOperations.sumaDeStats),
          Map(Stat(TipoStat.Fuerza,15) -> StatsOperations.sumaDeStats),
          Map(Stat(TipoStat.Inteligencia,10) -> StatsOperations.restaDeStats)
        )

        val guerrero = new Trabajo(TipoStat.Fuerza, statsAfectados)
        val ladron = new Trabajo(TipoStat.Fuerza, statsAfectados)
        val heroeBase = new Heroe(null, null, null, guerrero)

        // TODO: El trabajo del heroe deberia ser "Guerrero"

//        heroeBase.convertirseEn(ladron)

        // TODO: El trabajo del heroe deberia ser "Ladron"

        1 shouldBe 1
      }
//
//      "El heroe solo puede tener equipado un tipo de item a la vez" in {
//        var heroe1 = new Heroe()
//        1 shouldBe(1)
//      }
//
//
//      // probar si puede equiparse un item o no si cumple sus restricciones
//      "El heroe puede o no equiparse un item en base a las restricciones del mismo" in {
//        var heroe1 = new Heroe()
//        1 shouldBe(1)
//      }
//
//      "Si un héroe se equipa con un ítem para una parte del cuerpo que ya tiene ocupada, el ítem anterior se descarta" in {
//        var heroe1 = new Heroe()
//        1 shouldBe(1)
//      }

    }

  }

}
