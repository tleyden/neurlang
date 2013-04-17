Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronProcessTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.NeuronProcessState, as: NeuronProcessState

	test "neuron process produces output when enough inputs provided" do

    parameters = NeuronParameters.new(activation_function: function(:activation, 1))
		neuron_process_state = NeuronProcessState.new(parameters: parameters, 
																									input_nodes: [:fakepid, self()],
																								  output_nodes: [self()])
		{ :ok, pid } = NeuronProcess.start_link(neuron_process_state)

		NeuronProcess.process_input(pid, {:fakepid, 1})
		assert(receive_output() == :timeout)  # neuron still waiting for other input

		NeuronProcess.process_input(pid, {self(), 2})
		assert(receive_output() == 3)

	end


	def activation(inputs) do
		Enum.reduce inputs, 0, fn(x, acc) -> 
															 x + acc 
													 end 
	end


	def receive_output() do

		received = receive do
			{ :output, value} -> 
				value
		after
			1000 ->
				IO.puts "timeout waiting for message"
				:timeout
		end
		received

	end



end