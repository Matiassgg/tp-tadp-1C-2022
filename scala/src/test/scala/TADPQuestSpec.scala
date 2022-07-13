import TADPQuest._
import TADPQuest.ArmaduraEleganteSport
import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec

import scala.language.postfixOps

class TADPQuestSpec extends AnyFreeSpec {

  "TADPQuest Tests" - {

    "Tests de stats" - {

      "Se pueden crear stats" in {
        val stats: Stats = Stats(10,5,10,80)
        stats should not be null
      }

      "No se pueden crear stats negativos" in {
        assertThrows[IllegalArgumentException] {
          Stats(-1,0,0,0)
        }
      }
    }

    "Tests de Heroe" - {
      val stats: Stats = Stats(10, 10, 29, 29)
      val equipamiento: Equipamiento = Equipamiento(None, None, List.empty, List.empty)

      "Tests de Trabajo" - {
        val heroeBase = Heroe(stats, List.empty, equipamiento, Some(Guerrero))

        "El heroe tiene los stats modificados por el trabajo" in {
          heroeBase.HP shouldBe 20
          heroeBase.fuerza shouldBe 44
        }

        "Si el trabajo modifica un stat a un valor menor a 1, queda en 1" in {
          heroeBase.inteligencia shouldBe 1
        }

        "El heroe puede cambiar de trabajo" in {
          val heroeLadron: Heroe = heroeBase.convertirseEn(Ladron)
          heroeLadron.trabajo shouldBe Some(Ladron)
        }

        "El heroe cambia sus stats al cambiar de trabajo" in {
          heroeBase.HP shouldBe 10+10
          val heroeLadron: Heroe = heroeBase.convertirseEn(Ladron)
          heroeLadron.HP shouldBe 10-5
        }

        "El heroe puede cambiar a ningun trabajo" in {
          val heroeConvertido: Heroe = heroeBase.renunciar
          heroeConvertido.trabajo shouldBe None
        }
      }

      "Tests de Item" - {
        val statsBase: Stats = Stats(10, 5, 10, 5)
        val heroeConItems = Heroe(statsBase, List.empty, Equipamiento(None, Some(ArmaduraEleganteSport), List.empty, List.empty), None)

        "Los items de un heroe afectan a sus stats" in {
          heroeConItems.HP shouldBe 1
          heroeConItems.velocidad shouldBe 5+30
        }

        "Un heroe no puede equiparse con un item si no cumple con las restricciones del mismo" in {
          heroeConItems.equiparseCon(CascoVikingo)
          heroeConItems.equipamiento.cabeza shouldBe None
        }

        "Un heroe puede equiparse con un item si cumple con las restricciones del mismo" in {
          var heroeMago : Heroe = heroeConItems.convertirseEn(Mago)
          heroeMago.equipamiento.manos.size shouldBe(0)
          heroeMago = heroeMago.equiparseCon(PalitoMagico)
          heroeMago.equipamiento.manos.flatten.contains(PalitoMagico) shouldBe(true)
        }
      }
    }
  }
}
