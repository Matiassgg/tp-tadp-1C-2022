require_relative 'spec_helper'

describe Guerrero do
  let(:vida) { 100 }
  let(:fuerza) { 40 }
  let(:otro) { described_class.new(50, 90)}

  subject(:guerrero) { described_class.new(vida,fuerza) }

  context 'when initializes an instance' do
    it 'checks the invariant' do
      expect(described_class).to receive(:check_invariant).once
      guerrero
    end

    context 'if the invariant condition is not satisfied' do
      let(:fuerza) { 200 }

      it 'raises an error'  do #fails, waiting implementation
        expect{guerrero}.to raise_error(RuntimeError).with_message("invariant exception")
      end
    end
  end

  context 'after any instance method execution' do
    let(:method) { "atacar".to_sym }

    it 'checks the invariant' do
      expect(described_class).to receive(:check_invariant).at_least(:once)
      guerrero.send(method,otro)
    end

    context 'if the invariant condition is not satisfied' do
      let(:fuerza) { 60 }

      it 'raises an error' do
        expect{guerrero.send(method,otro)}.to raise_error(RuntimeError).with_message("invariant exception")
      end
    end
  end
end
