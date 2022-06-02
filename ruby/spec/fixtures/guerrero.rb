require_relative '../../lib/contrato'

class Guerrero
  include Contratos

  attr_accessor :vida, :fuerza

  invariant { vida >= 0 }
  invariant { fuerza > 0 && fuerza < 100 }

  before_and_after_each_call(
    proc { puts 'Entré a un mensaje 1' },
    proc { puts 'Salí de un mensaje 1' }
  )

  def initialize(vida, fuerza)
    @vida = vida
    @fuerza = fuerza
  end

  def atacar(otro)
    otro.vida -= fuerza
  end
end
