require_relative 'spec_helper'

describe Guerrero do
  let(:vida) { 100 }
  let(:fuerza) { 40 }
  let(:otro) { described_class.new(50, 90) }

  subject(:guerrero) { described_class.new(vida, fuerza) }

  context 'when initializes an instance' do
    it 'defines the invariants' do
      guerrero
      expect(described_class.instance_variable_get(:@invariants).size).to eq 2
    end

    context 'if the invariant condition is not satisfied' do
      let(:fuerza) { 200 }

      it 'raises an error' do
        expect { guerrero }.to raise_error(SystemExit).with_message('invariant exception')
      end
    end
  end

  context 'after any instance method execution' do
    let(:method) { 'atacar'.to_sym }

    it 'defines the invariants' do
      guerrero.send(method, otro)
      expect(described_class.instance_variable_get(:@invariants).size).to eq 2
    end

    context 'if the invariant condition is not satisfied' do
      let(:fuerza) { 60 }

      it 'raises an error' do
        expect { guerrero.send(method, otro) }.to raise_error(SystemExit).with_message('invariant exception')
      end
    end
  end
end
