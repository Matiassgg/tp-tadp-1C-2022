require_relative 'spec_helper'

describe Operaciones do
  context '#dividir' do
    subject { described_class.new }

    context 'precondition' do
      it 'defines the pre condition' do
        subject.send(:dividir, 10, 2)
        method_with_callbacks = described_class.instance_variable_get(:@methods_with_callbacks)[:dividir]
        expect(method_with_callbacks.instance_variable_get(:@precondition)).to be
      end

      it 'ok' do
        expect(subject.send(:dividir, 10, 2)).to eq(5)
      end

      it 'raises an error' do
        expect { subject.send(:dividir, 5, 0) }.to raise_error(SystemExit).with_message('precondition exception')
      end
    end

    context 'postcondition' do
      it 'defines the pre condition' do
        subject.send(:dividir, 10, 2)
        method_with_callbacks = described_class.instance_variable_get(:@methods_with_callbacks)[:dividir]
        expect(method_with_callbacks.instance_variable_get(:@postcondition)).to be
      end

      it 'ok' do
        expect(subject.send(:dividir, 10, 2)).to eq(5)
      end

      it 'raises an error' do
        expect do
          Operaciones.new.send(:dividir, 5, 4)
        end.to raise_error(SystemExit).with_message('postcondition exception')
      end
    end
  end

  context '#resta' do
    subject { described_class.new.send(:restar, 10, 2) }

    it 'not defines the pre condition' do
      subject
      method_with_callbacks = described_class.instance_variable_get(:@methods_with_callbacks)[:restar]
      expect(method_with_callbacks.instance_variable_get(:@precondition)).to be_nil
    end

    it 'not defines the post condition' do
      subject
      method_with_callbacks = described_class.instance_variable_get(:@methods_with_callbacks)[:restar]
      expect(method_with_callbacks.instance_variable_get(:@postcondition)).to be_nil
    end
  end
end
