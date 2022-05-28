class PrePost
  def initialize(contexto, params, args, block)
    params.zip(args).each do |param, arg|                     # [[:dividendo,4],[:dividor, 1]]
      #instance_variable_set("@#{param}", arg)
      contexto.define_singleton_method(param) do
        arg
      end
    end
    @block = block
    @contexto = contexto
  end

  def exec
    @contexto.instance_eval &@block
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
      puts "defini un pre #{expr}"
      @pre = expr
    end

    def post(&expr)
      puts "defini un post #{expr}"
      @post = expr
    end

    def exec_pre(contexto, params, *args)
      relations = params.zip(args)
      puts "entra a exec_pre"

      dividendo = 4
      divisor = 1
      #self.instance_variable_set("@#{params[0]}".to_sym, args[0]) # define dividendo con valor 4
      #self.instance_variable_set("@#{params[1]}".to_sym, args[1])
      #contexto.instance_eval &@pre unless @pre.nil?
      unless @pre.nil?
        raise "precondition exception" unless PrePost.new(contexto, params, args, @pre).exec
      end
    rescue RuntimeError => e
      abort(e.message)
    end

    def exec_post(contexto)
      puts "entra a exec_post y post es #{@post}"
      contexto.instance_exec &@post unless @post.nil?
      @post = nil
    end

    private

    def method_added(name)
      old_method = instance_method(name)
      parameters = old_method.parameters.map { |p| p[1] }
      __non_recursively__ do
        define_method(name) do |*args, &block|
          puts "exec pre con metodo #{name}"
          self.class.exec_pre(self, parameters, *args)
          self.class.exec_before_procs
          returned_values = old_method.bind(self).call(*args, &block)
          self.class.exec_after_procs
          puts "exec post con metodo #{name}"
          self.class.exec_post(self)
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
