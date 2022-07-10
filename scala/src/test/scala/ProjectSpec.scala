import TADPQuest.Stats
import TADPQuest.Heroe
import TADPQuest.Equipamiento
import TADPQuest.Guerrero
import TADPQuest.Ladron
import TADPQuest.CascoVikingo
import TADPQuest.ArmaduraEleganteSport
import TADPQuest.EspadaDeLaVida
import TADPQuest.TalismanMaldito
import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec

class ProjectSpec extends AnyFreeSpec {

  "TADPQuest Tests" - {

    "Tests de stats" - {
      "Se pueden crear stats" in {
        val stats: Stats = Stats(10,5,10,80)
        stats shouldNot be null
      }

      "No se pueden crear stats negativos" in {
        assertThrows[IllegalArgumentException] {
          Stats(-1,0,0,0)
        }
      }
    }

    "Tests de Heroe" - {
      val stats: Stats = Stats(10, 10, 29, 29)
      val equipamiento: Equipamiento = Equipamiento(CascoVikingo, ArmaduraEleganteSport, List(EspadaDeLaVida), List(TalismanMaldito))

      "Tests de Trabajo" - {
        val heroeBase = Heroe(stats, List.empty, equipamiento, Some(Guerrero))

        "El heroe puede cambiar de trabajo" in {

          val heroeLadron: Heroe = heroeBase.convertirseEn(Ladron)
          heroeLadron.trabajo shouldBe Ladron
        }

        "El heroe cambia sus stats al cambiar de trabajo" in {
          val heroeLadron: Heroe = heroeBase.convertirseEn(Ladron)
          heroeLadron.HP shouldBe 14
          heroeLadron.inteligencia shouldBe 10
        }

        "El heroe puede cambiar a ningun trabajo" in {
          val heroeConvertido: Heroe = heroeBase.convertirseEn(null)
          heroeConvertido.trabajo shouldBe null
        }
      }

      "Tests de Item" - {
        "Los items de un heroe afectan a sus stats" in {
          val stats2: Stats = Stats(10, 5, 10, 5)
          val heroeBase2 = Heroe(stats2, null, equipamiento, Some(Guerrero))
          heroeBase2.HP shouldBe 29
        }
      }
    }
  }
}
