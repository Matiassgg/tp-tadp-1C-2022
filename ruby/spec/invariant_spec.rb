require_relative 'spec_helper'

describe Guerrero do
  context 'invariant variable_random == 100' do
    it 'when initializes, variable_random has to be eq to 100' do
      expect(subject.instance_variable_get(:@variable_random)).to eql(100)
    end

    context 'after any method execution' do
      let(:method) { :cambiar_valor_random }

      it 'if the invariant condition is not satisfied raises an error' , skip: true do #fails, waiting implementation
        expect(subject.send(method)). to raise_error("invariant exception")
      end
    end
  end
end
