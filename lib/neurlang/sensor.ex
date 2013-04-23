
alias Neurlang.Accumulator, as: Accumulator
alias Neurlang.ConnectedNode, as: ConnectedNode
alias Neurlang.Sensor, as: Sensor

defrecord Neurlang.Sensor, id: nil, pid: nil, sync_function: nil, output_vector_length: nil, outbound_connections: []  do

  @moduledoc """
  Metadata for the Neuron node:

  * `id` - a unique id gotten from calling make_ref()

	* `pid` - the process id

	* `sync_function` - this function is called when the sensor receives a sync message.  
                      the return value is expected to be an input vector, and is forwarded
                      to all outbound connections.

  * `output_vector_length` - the sensors outputs vectors of this length

	* `outbound_connections` - a list of pid's of output nodes this sensor should send output to

  """
	record_type id: reference
	record_type pid: pid
	record_type sync_function: fun
	record_type output_vector_length: integer
	record_type outbound_connections: [pid]

end

defimpl Accumulator, for: Sensor do

	def create_barrier( node ) do 
		node
	end

	def update_barrier_state( node, {_from_pid, _input_value} ) do
		if node, do: throw "Sensors are not expected to receive inputs.  Use sync_function instead"
		node
	end

	def is_barrier_satisfied?( node ) do
		if node, do: throw "Sensors are not expected to receive inputs.  Use sync_function instead"
		node
	end

	def compute_output( node ) do
		if node, do: throw "Sensors are not expected to receive inputs.  Use sync_function instead"
		node
	end

	def propagate_output( node, output ) do
		message = { node.pid(), :forward, output }
		Enum.each node.outbound_connections(), fn(node) -> 
																								node <- message 
																						end
	end

	def sync( node ) do
		f = node.sync_function()
		f.( node )
	end

end

defimpl ConnectedNode, for: Sensor do

	def pid( node ) do
		node.pid()
	end

	def add_inbound_connection( node, _from_node ) do
		if node, do: throw "Sensors are not expected to have inbound connections.  Use sync_function instead"
		node
	end

	def add_inbound_connection( node, _from_node, _weights ) do
		if node, do: throw "Sensor inbound connections do not have weights associated with them"
		node
	end

	
	def add_outbound_connection( node, to_node ) do
		node.outbound_connections( [ to_node.pid() | node.outbound_connections() ] )
	end

end
