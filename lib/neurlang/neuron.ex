
alias Neurlang.Barrier, as: Barrier
alias Neurlang.Neuron, as: Neuron

defrecord Neurlang.Neuron, id: nil, pid: nil, activation_function: nil, bias: nil, 
										       inbound_connections: [], outbound_connections: [], barrier: HashDict.new do

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

defimpl Barrier, for: Neuron do

	def update_barrier_state(node, {from_pid, input_value}) do
		node.barrier( Dict.put(node.barrier(), from_pid, input_value) )
	end

	def is_barrier_satisfied?(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		inbound_connections_accounted = Enum.filter(inbound_connections, fn({pid, _weights}) -> HashDict.has_key?(barrier, pid) end)
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end


end