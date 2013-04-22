
alias Neurlang.Accumulator, as: Accumulator
alias Neurlang.Neuron, as: Neuron
alias Neurlang.NeuronHelper, as: NeuronHelper
alias Neurlang.ConnectedNode, as: ConnectedNode

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

defimpl Accumulator, for: Neuron do

	def update_barrier_state( Neuron[] = node, {from_pid, input_value} ) do
		node.barrier( Dict.put(node.barrier(), from_pid, input_value) )
	end

	def is_barrier_satisfied?( Neuron[inbound_connections: inbound_connections, barrier: barrier] ) do
		inbound_connections_accounted = Enum.filter(inbound_connections, fn({pid, _weights}) -> 
																																				 HashDict.has_key?(barrier, pid) 
																																		 end)
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end

	def compute_output( node ) do
		[ NeuronHelper.compute_output( node ) ]
	end

	def propagate_output( node, output ) do
		message = { node.pid(), :forward, output }
		Enum.each node.outbound_connections(), fn(node) -> 
																								node <- message 
																						end
	end


end

defimpl ConnectedNode, for: Neuron do

	def add_inbound_connection( node, from_node, weights ) do
		inbound_connection = { from_node.pid(), weights }
		node.inbound_connections( [ inbound_connection | node.inbound_connections() ] )
	end

	def add_inbound_connection( node, _from_node ) do 
		if node, do: throw "Neuron inbound connections must have weights associated with them"
		node
	end
	
	def add_outbound_connection( node, to_node ) do
		node.outbound_connections( [ to_node.pid() | node.outbound_connections() ] )
	end

end