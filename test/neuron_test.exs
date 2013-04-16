Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronTest do
  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess

  test "neuron produces output when process_input function called" do
		
		f = fn(_x) -> 1 end
    parameters = NeuronParameters.new(activation_function: f)
		inputs = [1,2]
		output = Neuron.process_input_vector(parameters, inputs)
		
		assert(output == 1)

	end

	test "neuron process produces output when enough inputs provided" do
		
		inbound_nodes = [self()]
		outbound_nodes = [self()]
		connected_nodes = [{:inbound_nodes, inbound_nodes}, {:outbound_nodes, outbound_nodes}]

		f = fn(_x) -> 1 end
    parameters = NeuronParameters.new(activation_function: f)
		{ :ok, pid } = NeuronProcess.start_link({parameters, connected_nodes})
		NeuronProcess.process_input(pid, {self(), 0.73})

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
