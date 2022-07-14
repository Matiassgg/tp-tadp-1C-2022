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
    }

    "Tests de Heroe" - {
      val stats: Stats = Stats(10, 10, 29, 29)
      val equipamiento: Equipamiento = Equipamiento(None, None, List.empty, List.empty)

      "Tests de Trabajo" - {
        val heroeBase = Heroe(stats, List.empty, equipamiento, Some(Guerrero))

        "No se pueden crear heroes con stats negativos" in {
          val stats: Stats = Stats(-1, 10, 29, 29)

          assertThrows[IllegalArgumentException] {
            val heroeBase = Heroe(stats, List.empty, equipamiento, Some(Guerrero))
          }
        }

        "El heroe tiene los stats modificados por el trabajo" in {
          heroeBase.hp shouldBe 20
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
          heroeBase.hp shouldBe 10+10
          val heroeLadron: Heroe = heroeBase.convertirseEn(Ladron)
          heroeLadron.hp shouldBe 10-5
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
          heroeConItems.hp shouldBe 1
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

    "Tests de Equipos" - {
      val goku = Heroe(Stats(100, 30, 100, 100), List.empty, Equipamiento(None, None, List.empty, List.empty), Some(Guerrero))
      val magoSinDientes = Heroe(Stats(30, 50, 5, 20), List.empty, Equipamiento(None, None, List(Some(PalitoMagico)), List.empty), Some(Mago))
      val macri = Heroe(Stats(40, 5, 10, 30), List.empty, Equipamiento(None, None, List.empty, List.empty), Some(Ladron))
      val tiktoker = Heroe(Stats(10, 20, 5, 50), List.empty, Equipamiento(None, None, List.empty, List.empty), None)
      val lukeSkywalker = Heroe(Stats(100, 100, 100, 100), List.empty, Equipamiento(Some(CascoVikingo), None, List.empty, List.empty), Some(Guerrero))

      val theBoys = Equipo("The boys", Set(goku, magoSinDientes, macri), 1000)

      "Podemos obtener el mejor heroe segun un cuantificador" in {
        def quienEsElMasFuerte = (heroe: Heroe) => heroe.fuerza

        theBoys.mejorHeroeSegun(quienEsElMasFuerte) shouldBe Some(goku)
      }

/*      "Cuando obtenemos un item se lo damos al que mas incrementa su stat principal" in {
        def itemIdealParaUnLadron = ArmaduraEleganteSport
        val macriEleganteSport = macri.equiparseCon(itemIdealParaUnLadron)

        println(macri.statPrincipal)
        println(macriEleganteSport.statPrincipal)

        theBoys.obtenerItem(ArmaduraEleganteSport).integrantes.contains(macriEleganteSport) shouldBe true
        theBoys.obtenerItem(ArmaduraEleganteSport).integrantes.contains(macri) shouldBe false
      }*/

      "Cuando obtenemos un item que no beneficia a nadie, se vende y suma el pozo comun" in {
        def item = ArmaduraEleganteSport
        val macriEleganteSport = macri.equiparseCon(ArmaduraEleganteSport)

        theBoys.obtenerItem(ArmaduraEleganteSport).pozoComun shouldBe theBoys.pozoComun + ArmaduraEleganteSport.valorVenta
      }

      "Se puede incorporar un nuevo miembro al equipo" in {
        theBoys.obtenerMiembro(tiktoker).integrantes.size shouldBe theBoys.integrantes.size + 1
      }

      "Se puede reemplazar un miembro del equipo por otro" in {
        val equipoModificado = theBoys.reemplazarMiembro(magoSinDientes, lukeSkywalker)
        equipoModificado.integrantes.contains(lukeSkywalker) shouldBe true
        equipoModificado.integrantes.contains(magoSinDientes) shouldBe false
      }

      "Se puede obtener el lider del equipo, quien es el que tiene el mayor stat principal" in {
        theBoys.lider shouldBe Some(goku)
      }

      "Si al calcular el lider hay empate entre stats principales, se considera que el equipo no tiene lider" in {
        theBoys.obtenerMiembro(lukeSkywalker).lider shouldBe None
      }
    }
  }
}
