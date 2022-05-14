require_relative '../../lib/contrato.rb'

class MiClase
  include Contratos

  def mensaje1
    puts 'mensaje_1'
    5
  end

  def mensaje2
    puts "mensaje_2"
    3
  end

  before_and_after_each_call(
    proc { puts 'Entré a un mensaje 1' },
    proc { puts 'Salí de un mensaje 1' }
  )

  before_and_after_each_call(
    proc { puts 'Entré a un mensaje 2' },
    proc { puts 'Salí de un mensaje 2' }
  )
end
