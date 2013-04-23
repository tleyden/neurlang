
alias Neurlang.Accumulator, as: Accumulator
alias Neurlang.Actuator, as: Actuator
alias Neurlang.ConnectedNode, as: ConnectedNode

defrecord Neurlang.Actuator, id: nil, pid: nil, inbound_connections: [], outbound_connections: [], 
										         barrier: HashDict.new do

  @moduledoc """
  Metadata for the Actuator node:

  * `id` - a unique id gotten from calling make_ref()

	* `pid` - the process id

	* `inbound_connections` - a list of pid's of neurons nodes this actuator should expect to receive input from

	* `outbound_connections` - a list of pid's of output nodes this actuator process should send output to. 
                             added for testing purposes, but could have other uses as well.

	* `barrier` - used to wait until receiving inputs from all connected input nodes before sending output

  """

	record_type id: reference
	record_type pid: pid
	record_type inbound_connections: [pid]
	record_type outbound_connections: [pid]
	record_type barrier: Dict

end

defimpl Accumulator, for: Actuator do

	def create_barrier( node ) do
		node.barrier( HashDict.new )
	end

	def update_barrier_state(node, {from_pid, input_value}) do
		node.barrier( Dict.put(node.barrier(), from_pid, input_value) )
	end

	def is_barrier_satisfied?(Actuator[inbound_connections: inbound_connections, barrier: barrier]) do
		inbound_connections_accounted = Enum.filter(inbound_connections, fn(pid) -> 
																																				 HashDict.has_key?(barrier, pid) 
																																		 end)
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end


	def compute_output( node ) do
			barrier = node.barrier()
			inbound_connections = node.inbound_connections()
			received_inputs = lc input_node_pid inlist inbound_connections, do: barrier[input_node_pid]
			List.flatten( received_inputs ) 
	end

	def propagate_output( node, output ) do
		message = { node.pid(), :forward, output }
		Enum.each node.outbound_connections(), fn(node) -> 
																								node <- message 
																						end
	end

	def sync( node ) do
		if node, do: throw "Actuators do not have sync functionality yet"
		node
	end


end


defimpl ConnectedNode, for: Actuator do

	def pid( node ) do
		node.pid()
	end

	def add_inbound_connection( node, _from_node, _weights ) do
		if node, do: throw "Actuator inbound connections do not have weights associated with them"
		node
	end

	def add_inbound_connection( node, from_node ) do 
		inbound_connection = from_node.pid()  
		node.inbound_connections( [ inbound_connection | node.inbound_connections() ] )
	end
	
	def add_outbound_connection( node, to_node ) do
		node.outbound_connections( [ to_node.pid() | node.outbound_connections() ] )
	end

end