require_relative '../../lib/contrato.rb'

class Guerrero
  include Contratos

  attr_accessor :variable_random

  invariant { variable_random == 100 }

  def initialize
    self.variable_random = 100
  end

  def cambiar_valor_random
    self.variable_random = 200
  end
end