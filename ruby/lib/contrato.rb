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

    def exec_pre(contexto)
      puts "entra a exec_pre"
      contexto.instance_eval(&@pre) unless @pre.nil?
      @pre = nil
    end

    def exec_post(contexto)
      puts "entra a exec_post y post es #{@post}"
      contexto.instance_eval(&@post) unless @post.nil?
      @post = nil
    end

    private

    def method_added(name)
      old_method = instance_method(name)
      __non_recursively__ do
        define_method(name) do |*args, &block|
          puts "exec pre con metodo #{name}"
          self.class.exec_pre(self)
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
