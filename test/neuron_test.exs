Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.NeuronProcessState, as: NeuronProcessState

  test "neuron produces output when process_input function called" do
		
		f = fn(inputs) -> Enum.reduce inputs, 0, fn(x, acc) -> x + acc end end
    parameters = NeuronParameters.new(activation_function: f)
		inputs = [1,2]
		output = Neuron.process_input_vector(parameters, inputs)
		
		assert(output == 3)

	end

	test "neuron process produces output when enough inputs provided" do

		f = fn(inputs) -> Enum.reduce inputs, 0, fn(x, acc) -> x + acc end end
    parameters = NeuronParameters.new(activation_function: f)
		neuron_process_state = NeuronProcessState.new(parameters: parameters, 
																									input_nodes: [self()],
																								  output_nodes: [self()])
		{ :ok, pid } = NeuronProcess.start_link(neuron_process_state)
		NeuronProcess.process_input(pid, {self(), 1})

		received = receive do
			{ :output, value} -> 
				IO.puts "received value: #{value}"
				:ok
		after
			1000 ->
				IO.puts "timeout waiting for message"
				:timeout
		end
		
		assert(received == :ok)

	end
	
end
