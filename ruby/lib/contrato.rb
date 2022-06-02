require_relative '../lib/pre_post'
require_relative '../lib/method_with_callbacks'

module Contratos
  def self.included(klass)
    initialize_contracts_attrs(klass)
    klass.extend(ContractsClassMethods)
  end

  def self.initialize_contracts_attrs(klass)
    klass.instance_eval do
      @procs_before = []
      @procs_after = []
      @invariants = []
      @dispatcher = {}
      @pre = nil
      @post = nil
    end
  end

  module ContractsClassMethods
    def before_and_after_each_call(proc_before, proc_after)
      @procs_before << proc_before
      @procs_after << proc_after
    end

    def invariant(&expr)
      @invariants << expr
    end

    def pre(&expr)
      @pre = expr
    end

    def post(&expr)
      @post = expr
    end

    private

    def method_added(name)
      old_method = instance_method(name)
      __non_recursively__ do
        precondition = @pre
        postcondition = @post
        original_class = self

        # Faltaría chequear si ya existe?
        @dispatcher[name] = MethodWithCallbacks.new(name, old_method, original_class, precondition, postcondition)

        define_method(name) do |*args, &block|
          instance = self
          original_class.instance_variable_get(:@dispatcher)[name].call(instance, *args, &block)
        end
        @pre = nil
        @post = nil
      end
    end

    def __non_recursively__
      return if Thread.current[:executing_contract_define_method]

      Thread.current[:executing_contract_define_method] = true
      yield
      Thread.current[:executing_contract_define_method] = false
    end
  end
end
