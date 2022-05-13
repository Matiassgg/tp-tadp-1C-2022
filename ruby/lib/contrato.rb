
module Contratos
    def self.included(klass)
        initialize_contracts_attrs(klass)
        klass.extend(ContractsClassMethods)
    end

    def self.initialize_contracts_attrs(klass)
        klass.instance_eval do
            @procs_before = []
            @procs_after = []
        end
    end

    module ContractsClassMethods
        def before_and_after_each_call(proc_before,proc_after)
            puts "valor de self "  + self.inspect
            puts "valor de class " + self.class.inspect

            @procs_before << proc_before
            @procs_after << proc_after
        end

        def __non_recursively__
            return if Thread.current[:executing_contract_define_method]

            Thread.current[:executing_contract_define_method] = true
            yield
            Thread.current[:executing_contract_define_method] = false
        end
    end
end

# TODO: ver de deprecar esto por un method_added con define_method..
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

# # 1° requerimiento
# instancia = MiClase.new
# clase_wrapper = Wrapper.new(instancia)
# clase_wrapper.mensaje_1
# res = clase_wrapper.mensaje_2
# puts "obtengo res = #{res}"
# class MiClase
#
# def nuevo_metodo
#     puts "nuevo_metodo"
# end
# end
#
# clase_wrapper.nuevo_metodo