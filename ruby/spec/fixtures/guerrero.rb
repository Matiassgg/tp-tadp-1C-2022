require_relative '../../lib/contrato.rb'

class Guerrero
  include Contratos

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