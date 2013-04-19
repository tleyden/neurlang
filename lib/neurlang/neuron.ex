

defrecord Neurlang.Neuron, id: nil, pid: nil, activation_function: nil, bias: nil, inbound_connections: [], outbound_connections: [], barrier: nil do

  @moduledoc """
  Metadata for the Neuron node:

  * `id` - a unique id gotten from calling make_ref()

	* `pid` - the process id

  * `activation_function` - a function which will be used to calculate the output, eg, a sigmoid funtion

  * `bias` - after dot product of input vector and weight vector is taken, a scalar bias value is added

	* `inbound_connections` - a list of {pid, weight_vector} tuples representing inbound connections

	* `outbound_connections` - a list of pid's of output nodes this neuron process should send output to

	* `barrier` - used to wait until receiving inputs from all connected input nodes before sending output

  """

end

defmodule Neurlang.NeuronHelper do

	alias Neurlang.Neuron, as: Neuron

	def compute_output(neuron) do
		Neuron[activation_function: f, bias: bias] = neuron
		inputs = get_inputs(neuron)
		# TODO: get weights from inbound_connections ..
		42
	end

	defp get_inputs(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		"""
    Get the inputs that will be fed into neuron, which are stored in the now-full barrier.
		They must be in the same order as the keys of the input_nodes.
		"""
		lc {input_node_pid, _weights} inlist inbound_connections, do: barrier[input_node_pid]
	end

  
end

