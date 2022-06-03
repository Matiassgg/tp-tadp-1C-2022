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

    it 'will define before procs' do
      subject.send(method)
      expect(described_class.instance_variable_get(:@procs_before).size).to eq 2
    end

    it 'will define after procs' do
      subject.send(method)
      expect(described_class.instance_variable_get(:@procs_after).size).to eq 2
    end
  end

  # VER QUE TESTEAR AHORA QUE EL METODO NO ES DE LA CLASE
  #
  # context 'Any non defined method' do
  #   let(:method) { %i[nil? class methods].sample } # singleton_method
  #
  #   it 'will not  define before procs before procs' do
  #     expect(described_class).to_not receive(:exec_before_procs)
  #     subject.send(method)
  #   end
  #
  #   it 'will not define before procs after procs' do
  #     expect(described_class).to_not receive(:exec_after_procs)
  #     subject.send(method)
  #   end
  # end
  #
  #   context 'Opening the class' do
  #     before do
  #       class MiClase
  #         @proc = lambda { puts 'Test Before' }
  #         @proc2 = lambda { puts 'Test After' }
  #
  #         before_and_after_each_call(@proc, @proc2)
  #       end
  #     end
  #
  #     it 'will execute the new hooks' do
  #       expect(described_class).to receive(:before_and_after_each_call)
  #       .with(lambda { puts 'Test Before' }, lambda { puts 'Test After' })
  #       expect(described_class.instance_variable_get(:@procs_before).last.to_json).to
  #       eq({lambda { puts 'Test Before' }}.to_json)
  #     end
  #   end
end
