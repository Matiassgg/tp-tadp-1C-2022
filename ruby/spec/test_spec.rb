require_relative 'spec_helper'

describe MiClase do
  let(:method) { %i[mensaje1 mensaje2].sample }

  it '#mensaje1 returns 5' do
    expect(subject.mensaje1).to be 5
  end

  it '#mensaje2 returns 3' do
    expect(subject.mensaje2).to be 3
  end

  it 'Any defined method have an before hook call' do
    expect(subject.class).to receive(:exec_before_procs).once
    subject.send(method)
  end

  it 'Any defined method have an after hook call' do
    expect(subject.class).to receive(:exec_after_procs).once

    subject.send(method)
  end
end
