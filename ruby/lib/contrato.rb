class PrePost
  def initialize(contexto, params, args, block, result = nil)
    params.zip(args).each do |param, arg|
      contexto.define_singleton_method(param) do
        arg
      end
    end
    @block = block
    @contexto = contexto
    @result = result
  end

  def exec
    if @result.nil?
      @contexto.instance_eval &@block
    else
      @contexto.instance_exec(@result, &@block)
    end
  end
end

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
      @pre = nil
      @post = nil
    end
  end

  module ContractsClassMethods
    def before_and_after_each_call(proc_before, proc_after)
      @procs_before << proc_before
      @procs_after << proc_after
    end

    def exec_before_procs
      @procs_before.each(&:call)
    end

    def exec_after_procs
      @procs_after.each(&:call)
    end

    def check_invariant(contexto)
      @invariants.each do |invariante|
        raise "invariant exception"  unless contexto.instance_eval(&invariante)
      end
    rescue RuntimeError => e
      abort(e.message)
    end

    def invariant(&expr)
      @invariants << expr
    end

    def pre(&expr)
      @pre = expr
    end

    def post(&expr)
      puts "defini un post #{expr}"
      @post = expr
    end

    def exec_pre(contexto, params, *args)
      unless @pre.nil?
        raise "precondition exception" unless PrePost.new(contexto, params, args, @pre).exec
      end
    rescue RuntimeError => e
      abort(e.message)
    end

    def exec_post(contexto, params, *args, result)
      unless @post.nil?
        raise "postcondition exception" unless PrePost.new(contexto, params, args, @post, result).exec
      end
    rescue RuntimeError => e
      abort(e.message)
    end

    private

    def method_added(name)
      old_method = instance_method(name)
      parameters = old_method.parameters.map { |p| p[1] }
      __non_recursively__ do
        define_method(name) do |*args, &block|
          self.class.exec_pre(self, parameters, *args)
          self.class.exec_before_procs
          returned_values = old_method.bind(self).call(*args, &block)
          self.class.exec_after_procs
          self.class.exec_post(self, parameters, *args, returned_values)
          self.class.check_invariant(self) unless self.respond_to?("#{name.to_s}=")
          returned_values
        end
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
