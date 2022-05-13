require_relative 'spec_helper'

describe 'suite de tests para 1° requerimiento' do
    it 'Se mandan los mensajes de la ejecucion' do
        clase_test = MiClase.new
        # Debugging
        # binding.pry
        # expect(clase_test.mensaje_1).to be 5
        # expect(clase_test.mensaje_2).to be 3
    end
end

