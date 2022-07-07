import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec

class ProjectSpec extends AnyFreeSpec {

  "TADPQuest Tests" - {

    "Tests de stats" - {
      "Se pueden crear stats" in {
        val hp: HP = HP(10)
        val inteligencia: Inteligencia = Inteligencia(10)
        val velocidad: Velocidad = Velocidad(29)
        val fuerza: Fuerza = Fuerza(29)
        val stats: Stats = Stats(hp, inteligencia, velocidad, fuerza)
        stats.HP.value shouldBe 10
      }

      "No se pueden crear stats negativos" in {
        assertThrows[IllegalArgumentException] {
          val _: HP = HP(-10)
        }
      }
    }

    "Tests de Heroe" - {
      val hp: HP = HP(10)
      val inteligencia: Inteligencia = Inteligencia(10)
      val velocidad: Velocidad = Velocidad(29)
      val fuerza: Fuerza = Fuerza(29)
      val stats: Stats = Stats(hp, inteligencia, velocidad, fuerza)
      val guerrero = new Trabajo(fuerza: Fuerza, Incrementos(10, 15, -10))
      val item_cabeza: Item = Item(Cabeza, Incrementos(1, 0, 0, 0), null)
      val item_torso: Item = Item(Torso, Incrementos(2, 0, 0, 0), null)
      val item_manos: List[Item] = List(new Item(Mano, Incrementos(3, 0, 0, 0), null))
      val item_talismanes: List[Item] = List(new Item(Talisman, Incrementos(3, 0, 0, 0), null))
      val equipamiento: Equipamiento = new Equipamiento(item_cabeza, item_torso, item_manos, item_talismanes)
      val ladron = new Trabajo(velocidad: Velocidad, Incrementos(-5, 0, 0, 10))

      "Tests de Trabajo" - {
        val heroeBase = Heroe(stats, null, equipamiento, guerrero)

        "El heroe puede cambiar de trabajo" in {

          val heroeLadron: Heroe = heroeBase.convertirseEn(ladron)
          heroeLadron.trabajo shouldBe ladron
        }

        "El heroe cambia sus stats al cambiar de trabajo" in {
          val heroeLadron: Heroe = heroeBase.convertirseEn(ladron)
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
          val stats2: Stats = Stats(hp, inteligencia, velocidad, fuerza)
          val heroeBase2 = Heroe(stats2, null, equipamiento, guerrero)
          heroeBase2.HP shouldBe 29
        }
      }
    }
  }
}
