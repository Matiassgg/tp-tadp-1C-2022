module Contrato
    def before_and_after_each_call(proc_before,proc_after)
        if self.instance_variable_get(:@procs_before) == nil
            self.instance_variable_set(:@procs_before,Array.new)
        end
        if self.instance_variable_get(:@procs_after) == nil
            self.instance_variable_set(:@procs_after,Array.new)
        end
        procs_before = self.instance_variable_get(:@procs_before)
        procs_before << proc_before
        procs_after = self.instance_variable_get(:@procs_after)
        procs_after << proc_after
    end


end

class MiClase
    include Contrato
    
    def mensaje_1
        puts "mensaje_1"
        return 5
    end

    def mensaje_2
        puts "mensaje_2"
        return 3
    end

    def initialize
        before_and_after_each_call(
            proc { puts "Entré a un mensaje 1" }, 
            proc { puts "Salí de un mensaje 1" }
        )
        
        # before_and_after_each_call(
        #     proc { puts "Entré a un mensaje 2" }, 
        #     proc { puts "Salí de un mensaje 2" }
        # )
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
        procs_before = @instancia_asociada.instance_variable_get(:@procs_before)
        procs_before.each do |proc_before|
            proc_before.call
        end

        value_return = @instancia_asociada.send(nombre_metodo,*args,&bloque)

        # Leer todos los procs after de la lista y ejecutarlos
        procs_after = @instancia_asociada.instance_variable_get(:@procs_after)
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

