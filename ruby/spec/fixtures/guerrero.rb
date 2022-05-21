require_relative '../../lib/contrato.rb'

class Guerrero
  include Contratos

=begin
  attr_accessor :variable_random

  invariant { variable_random == 100 }

  def initialize
    self.variable_random = 100
  end

  def cambiar_valor_random
    self.variable_random = 200
  end
=end

  attr_accessor :vida, :fuerza

  invariant { vida >= 0 }
  invariant { fuerza > 0 && fuerza < 100 }

  def initialize(vida, fuerza)
    @vida = vida
    @fuerza = fuerza
  end

  def atacar(otro)
    otro.vida -= fuerza
  end
end