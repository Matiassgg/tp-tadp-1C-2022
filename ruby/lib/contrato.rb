# frozen_string_literal: true

require_relative 'pre_post'
require_relative 'contract_executor'
require_relative 'overrides'


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
      @methods_with_callbacks = {}
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

    def add_contract_executor_for_signature(contract_executor, signature_name, override: false)
      return if get_contract_executor(signature_name).present? && !override

      @methods_with_callbacks[signature_name] = contract_executor
    end

    def get_contract_executor(signature_name)
      @methods_with_callbacks[signature_name]
    end

    def pre_post_cleanup
      @pre = nil
      @post = nil
    end

    private

    def method_added(name)
      super
      old_method = instance_method(name)
      __non_recursively__ do
        original_class = self

        # Faltaría chequear si ya existe?
        contract_executor = ContractExecutor.new(name, old_method, original_class, @pre, @post)
        add_contract_executor_for_signature(contract_executor, name)

        define_method(name) do |*args, &block|
          instance = self
          original_class.get_contract_executor(name).call(instance, *args, &block)
        end
        pre_post_cleanup
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
