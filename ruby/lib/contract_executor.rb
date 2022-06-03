# frozen_string_literal: true

# Contract executor class, to execute pre/post conditions and invariants over an instance
class ContractExecutor
  attr_reader :method_name, :old_method, :original_class, :precondition, :postcondition, :instance

  def initialize(method_name, old_method, original_class, precondition = nil, postcondition = nil)
    @method_name = method_name
    @old_method = old_method
    @original_class = original_class
    @precondition = precondition
    @postcondition = postcondition
  end

  def call(instance, *args, &block)
    @instance = instance
    exec_pre(*args) if precondition
    exec_before_procs
    returned_values = old_method.bind(instance).call(*args, &block)
    exec_after_procs
    exec_post(*args, returned_values) if postcondition
    check_invariant unless instance.respond_to?(method_name_setter)
    returned_values
  end

  private

  def method_name_setter
    "#{method_name}="
  end

  def params
    @old_method.parameters.map { |p| p[1] }
  end

  def exec_pre(*args)
    raise 'precondition exception' unless PrePost.new(instance, params, args.to_a, precondition).exec
  rescue RuntimeError => e
    abort(e.message)
  end

  def exec_before_procs
    original_class.instance_variable_get(:@procs_before).each(&:call)
  end

  def exec_after_procs
    original_class.instance_variable_get(:@procs_after).each(&:call)
  end

  def exec_post(*args, result)
    raise 'postcondition exception' unless PrePost.new(instance, params, args.to_a, postcondition, result).exec
  rescue RuntimeError => e
    abort(e.message)
  end

  def check_invariant
    original_class.instance_variable_get(:@invariants).each do |invariant|
      raise 'invariant exception' unless instance.instance_eval(&invariant)
    end
  rescue RuntimeError => e
    abort(e.message)
  end
end
