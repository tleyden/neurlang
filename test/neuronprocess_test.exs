Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronProcessTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.NeuronProcessState, as: NeuronProcessState

	test "neuron process produces output when enough inputs provided" do

		weights = [2,2]
		bias = 100
    parameters = NeuronParameters.new(activation_function: function(:activation, 3),
																		  weights: weights,
																		  bias: bias)

		neuron_process_state = NeuronProcessState.new(parameters: parameters, 
																									input_nodes: [:fakepid, self()],
																								  output_nodes: [self()])
		{ :ok, neuron_pid } = NeuronProcess.start_link(neuron_process_state)

		NeuronProcess.send_input(neuron_pid, {:fakepid, 1})
		assert(receive_output() == :timeout)  # neuron still waiting for other input

		NeuronProcess.send_input(neuron_pid, {:unlisted_pid, 10})
		assert(receive_output() == :timeout)  # doesn't know about this pid, should ignore

		NeuronProcess.send_input(neuron_pid, {self(), 2})
		assert(receive_output() == 107)

	end


	def activation(inputs, weights, bias) do
		sum_inputs = Enum.reduce inputs, 0, fn(x, acc) -> 
																						x + acc 
																				end 
		sum_weights = Enum.reduce weights, 0, fn(x, acc) -> 
																							x + acc 
																					end 
		sum_inputs + sum_weights + bias
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