Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.NeuronProcessState, as: NeuronProcessState
	alias Neurlang.TestHelper, as: TestHelper

  test "neuron produces output when process_input function called" do
		
		weights = [2,2]
		bias = 100
    parameters = NeuronParameters.new(activation_function: function(:summation, 3),
																		  weights: weights,
																		  bias: bias)
		inputs = [1,2]
		output = Neuron.process_input_vector(parameters, inputs)
		
		assert(output == 107)

	end

	def summation(inputs, weights, bias) do
		TestHelper.summation(inputs, weights, bias)
	end

	
end
