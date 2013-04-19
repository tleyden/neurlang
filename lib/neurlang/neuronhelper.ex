
defmodule Neurlang.NeuronHelper do
	@moduledoc """
  Functionality of the nueron that is unrelated to process/communication/otp.
  """
	alias Neurlang.Neuron, as: Neuron

	def compute_output(neuron) do
		Neuron[activation_function: activation_function, bias: bias] = neuron
		weighted_inputs = get_weighted_inputs(neuron)
		IO.puts "weighted_inputs: #{inspect(weighted_inputs)}"
		dot_product = bias
		Enum.each weighted_inputs, fn(weighted_input) ->
																	 {inputs, weights} = weighted_input
																	 dot_product = dot_product + dot_product(inputs, weights)  # <-- TODO!
															 end
		activation_function.(dot_product)
	end

	defp get_weighted_inputs(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		"""
    Get the inputs that will be fed into neuron, which are stored in the now-full barrier.
    Returns a list of the form [{input_vector,weight_vector}, ...]
		"""
		lc {input_node_pid, weights} inlist inbound_connections do
			inputs = barrier[input_node_pid]
			if length(inputs) != length(weights) do 
				throw "length of inputs #{inspect(inputs)} != length of weights #{inspect(weights)}"
			end
			{ inputs, weights } 
		end

	end

	def dot_product(inputs, weights) do
		dot_product(inputs, weights, 0)
	end

	defp dot_product([i|inputs], [w|weights], acc) do
		dot_product(inputs, weights, i*w + acc)
	end

	defp dot_product([], [], acc) do
		acc
	end
  
end

