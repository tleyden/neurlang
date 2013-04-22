
alias Neurlang.Barrier, as: Barrier
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

end

defimpl Barrier, for: Actuator do

	def update_barrier_state(node, {from_pid, input_value}) do
		node.barrier( Dict.put(node.barrier(), from_pid, input_value) )
	end

	def is_barrier_satisfied?(Actuator[inbound_connections: inbound_connections, barrier: barrier]) do
		IO.puts "is_barrier_satisfied: #{inspect(inbound_connections)}"
		inbound_connections_accounted = Enum.filter(inbound_connections, fn(pid) ->  # TODO: change to  fn({pid, _weights}
																																				 HashDict.has_key?(barrier, pid) 
																																		 end)
		IO.puts "actuator.is_barrier_satisfied called.  ic: #{inspect(inbound_connections)}"
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end

end


defimpl ConnectedNode, for: Actuator do

	def add_inbound_connection( node, from_node, weights ) do
		if node, do: throw "Actuator inbound connections do not have weights associated with them"
		node
	end

	def add_inbound_connection( node, from_node ) do 
		IO.puts "actuator.add_inbound node: #{inspect(node)} from_node: #{inspect(from_node)}"
		# TODO: change to this:
		# weights = []
		# inbound_connection = { from_node.pid(), weights }  
		inbound_connection = from_node.pid()  
		IO.puts "inbound_connection: #{inspect(inbound_connection)}"
		node.inbound_connections( [ inbound_connection | node.inbound_connections() ] )
	end
	
	def add_outbound_connection( node, to_node ) do
		node.outbound_connections( [ to_node.pid() | node.outbound_connections() ] )
	end

end