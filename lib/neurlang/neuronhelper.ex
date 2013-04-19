
defmodule Neurlang.NeuronHelper do
	@moduledoc """
  Functionality of the nueron that is unrelated to process/communication/otp.
  """
	alias Neurlang.Neuron, as: Neuron

	@doc """
	Compute the output for this neuron based on its parameters (bias, activation function)
	and the inputs/weights tuples stored in the barrier structure, which is presumed to
  be full with inputs from all inbound nodes.
	"""
	def compute_output(neuron) do
		Neuron[activation_function: activation_function, bias: bias] = neuron
		weighted_inputs = get_weighted_inputs(neuron)
		IO.puts "compute_output called with: #{inspect(neuron)} weighted_iinputs: #{inspect(weighted_inputs)}"
		compute_output(weighted_inputs, bias, activation_function)
	end

	@doc false
	def compute_output(weighted_inputs, bias, activation_function) do
		reduce_function = fn({inputs, weights}, acc) -> 
													dot_product(inputs, weights) + acc 
											end
		output = Enum.reduce weighted_inputs, 0, reduce_function 
		output = output + bias
		activation_function.(output)
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

