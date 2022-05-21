require_relative 'spec_helper'

describe Guerrero do
  let(:vida) { 100 }
  let(:fuerza) { 40 }
  let(:otro) { described_class.new(50, 90)}

  subject(:guerrero) { described_class.new(vida,fuerza) }

  context 'when initializes an instance' do
    it 'checks the invariant', skip: true do
      subject
      expect(described_class).to receive(:check_invariant).once
    end

    context 'if the invariant condition is not satisfied' do
      let(:fuerza) { 200 }

      it 'raises an error' , skip: true do #fails, waiting implementation
        expect(subject).to raise_error("invariant exception")
      end
    end
  end

  context 'after any instance method execution' do
    let(:method) { :atacar }

    it 'checks the invariant', skip: true do
      expect(described_class).to receive(:check_invariant).once
      subject.send(method,otro)
    end

    context 'if the invariant condition is not satisfied' do
      let(:fuerza) { 60 }

      it 'raises an error' , skip: true do #fails, waiting implementation
        expect(subject.send(method,otro)). to raise_error("invariant exception")
      end
    end
  end
end
