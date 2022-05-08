
module Contrato
    def before_and_after_each_call(proc_before,proc_after)
        puts "valor de self "  + self.inspect
        puts "valor de class " + self.class.inspect

        self.class_variable_set(:@@procs_before, Array.new) unless self.class_variable_defined?(:@@procs_before)
        self.class_variable_set(:@@procs_after,Array.new) unless self.class_variable_defined?(:@@procs_after)

        self.class_variable_get(:@@procs_before) << proc_before
        self.class_variable_get(:@@procs_after) << proc_after
    end
end

class Class
    include Contrato
end

class MiClase

    def mensaje_1
        puts "mensaje_1"
        return 5
    end

    def mensaje_2
        puts "mensaje_2"
        return 3
    end

    before_and_after_each_call(
      proc { puts "Entré a un mensaje 1" },
      proc { puts "Salí de un mensaje 1" }
    )

    before_and_after_each_call(
      proc { puts "Entré a un mensaje 2" },
      proc { puts "Salí de un mensaje 2" }
    )

    def initialize

    end

    # Con el wrapper no es necesario
    def self.method_added(nombre)
        puts "se agrega el metodo #{nombre}"
    end

end

class Wrapper

    def initialize(instancia)
        @instancia_asociada = instancia
    end

    def method_missing(nombre_metodo,*args,&bloque)
        # Leer todos los procs before de la lista y ejecutarlos
        procs_before = @instancia_asociada.class.class_variable_get(:@@procs_before)
        # puts "los proc son"
        # puts @instancia_asociada
        # puts @instancia_asociada.class

        procs_before.each do |proc_before|
            proc_before.call
        end

        value_return = @instancia_asociada.send(nombre_metodo,*args,&bloque)

        # Leer todos los procs after de la lista y ejecutarlos
        procs_after = @instancia_asociada.class.class_variable_get(:@@procs_after)
        procs_after.each do |proc_after|
            proc_after.call
        end

        return value_return
    end

end

# 1° requerimiento
instancia = MiClase.new
clase_wrapper = Wrapper.new(instancia)
clase_wrapper.mensaje_1
res = clase_wrapper.mensaje_2
puts "obtengo res = #{res}"
class MiClase

def nuevo_metodo
    puts "nuevo_metodo"
end
end

clase_wrapper.nuevo_metodo