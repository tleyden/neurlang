
defmodule Neurlang.Neuron do

	@moduledoc """
  A neuron
  """
	
	@doc """
  Process the given input vector according to parameters and return output value
  """
	def process_input_vector(neuron_parameters, inputs) do
		
		activation_function = neuron_parameters.activation_function()
		weights = neuron_parameters.weights()
		bias = neuron_parameters.bias()

		activation_function.(inputs, weights, bias)

	end


	@ doc """
  Calculate the dot product
  """
	def dot([i|inputs], [w|weights], accumulator) do
		dot(inputs, weights, i*w + accumulator)
	end

	def dot([], [], accumulator) do
		accumulator
	end


end