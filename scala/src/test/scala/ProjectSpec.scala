import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.{AnyFreeSpec}

class ProjectSpec extends AnyFreeSpec {

  "TADPQuest Tests" - {

    "Tests de Heroe" - {
      "Obtener stats" in {
        val heroe1 = new Heroe()
//        heroe1.leerStats should(containWord = "Fuerza")
      }

      "Equipar un item" in {
        //        Prueba.materia shouldBe "tadp"
        var heroe1 = new Heroe()
      }

      "Cambiar de trabajo" in {
        //        Prueba.materia shouldBe "tadp"
        var heroe1 = new Heroe()
      }
    }

    "Tests de Equipo" - {
      "Realizar mision" in {

        val equipo = new Equipo(Array(new Heroe(), new Heroe()))

//        equipo.realizarMision(new Mision(Array(new Tarea(), new Tarea()))) match {
//          case
//        }
//
//        heroe1.leerStats should(containWord = "Fuerza")
      }
    }
  }

}
