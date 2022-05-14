require_relative 'spec_helper'

describe 'suite de tests para 1° requerimiento' do
  let(:mi_clase) { MiClase.new }

  it 'Mensaje 1' do
    expect(mi_clase.mensaje_1).to be 5
  end

  it 'mensaje 2' do
    expect(mi_clase.mensaje_2).to be 3
  end
end

