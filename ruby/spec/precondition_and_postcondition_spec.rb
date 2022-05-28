require_relative 'spec_helper'

describe Operaciones do
  context '#dividir' do
    subject { described_class.new }

    it 'check pre' do
      expect(described_class).to receive(:exec_pre).once
      subject.send(:dividir, *[5,4])
    end

    it 'ok' do
      expect(subject.send(:dividir, *[10,2])).to eq(5)
    end

    it 'raises an error' do
      expect{subject.send(:dividir, *[5,0])}.to raise_error(SystemExit).with_message("precondition exception")
    end
  end
end
