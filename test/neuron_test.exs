Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.NeuronProcessState, as: NeuronProcessState

  test "neuron produces output when process_input function called" do
		
		weights = [2,2]
		bias = 100
    parameters = NeuronParameters.new(activation_function: function(:activation, 3),
																		  weights: weights,
																		  bias: bias)
		inputs = [1,2]
		output = Neuron.process_input_vector(parameters, inputs)
		
		assert(output == 107)

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

	
end
