
defmodule Neurlang.Neuron do
	@moduledoc """
  A neuron
  """
	def process_input_vector(NeuronParameters[activation_function: f], inputs) do
		f.(inputs)
	end


end