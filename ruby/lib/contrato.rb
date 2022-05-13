
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
            @procs_before << proc_before
            @procs_after << proc_after
        end

        def exec_before_procs
            @procs_before.each { |proc_before| proc_before.call }
        end

        def exec_after_procs
            @procs_after.each { |proc_after| proc_after.call }
        end

        def method_added(name)
            puts "Se llama method added de #{name}"
            old_method = instance_method(name)
            puts "Self en method added: #{self}"
            __non_recursively__ do
                define_method(name) do |*args, &block|
                    puts "Entro a define_method"
                    self.class.exec_before_procs
                    old_method.bind(self).call(*args, &block)
                    self.class.exec_after_procs
                end
            end
        end

        def __non_recursively__
            return if Thread.current[:executing_contract_define_method]
            puts "entré a non recursively"
            Thread.current[:executing_contract_define_method] = true
            puts "non recursively --> true"
            yield
            Thread.current[:executing_contract_define_method] = false
            puts "non recursively --> false"
        end
    end
end
