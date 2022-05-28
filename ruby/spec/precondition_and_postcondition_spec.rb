require_relative 'spec_helper'

describe Operaciones do
  context '#dividir' do
    subject { described_class.new }

    context 'precondition' do
      it 'check pre' do
        expect(described_class).to receive(:exec_pre).once
        subject.send(:dividir, *[10,2])
      end

      it 'ok' do
        expect(subject.send(:dividir, *[10,2])).to eq(5)
      end

      it 'raises an error' do
        expect{subject.send(:dividir, *[5,0])}.to raise_error(SystemExit).with_message("precondition exception")
      end
    end

    context 'postcondition' do
      it 'check post condition' do
        expect(described_class).to receive(:exec_post).once
        subject.send(:dividir, *[10,2])
      end

      it 'ok' do
        expect(subject.send(:dividir, *[10,2])).to eq(5)
      end

      it 'raises an error' do
        expect{Operaciones.new.send(:dividir, *[5,4])}.to raise_error(SystemExit).with_message("postcondition exception")
      end
    end
  end

  context '#resta' do
    subject { described_class.new.send(:restar, *[10,2]) }

    it 'no llama a pre condition' do
      expect(described_class).to_not receive(:exec_pre)
      subject
    end

    it 'no llama a post condition' do
      expect(described_class).to_not receive(:exec_post)
      subject
    end
  end
end
