require_relative 'spec_helper'

describe MiClase do
  it '#mensaje1 returns 5' do
    expect(subject.mensaje1).to be 5
  end

  it '#mensaje2 returns 3' do
    expect(subject.mensaje2).to be 3
  end

  context 'Any defined method' do
    let(:method) { %i[mensaje1 mensaje2].sample }

    it 'will have an execution of the before procs' do
      expect(subject.class).to receive(:exec_before_procs).once
      subject.send(method)
    end

    it 'will have an execution of the after procs' do
      expect(subject.class).to receive(:exec_after_procs).once
      subject.send(method)
    end
  end

  context 'Any non defined method' do
    let(:method) { %i[nil? class singleton_method methods].sample }

    it 'will not have an execution of the before procs' do
      expect(subject.class).to_not receive(:exec_before_procs)
      subject.send(method)
    end

    it 'will not have an execution of the after procs' do
      expect(subject.class).to_not receive(:exec_after_procs)
      subject.send(method)
    end
  end
end
