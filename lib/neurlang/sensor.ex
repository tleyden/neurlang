
alias Neurlang.ConnectedNode, as: ConnectedNode
alias Neurlang.Accumulator, as: Accumulator
alias Neurlang.Sensor, as: Sensor

defrecord Neurlang.Sensor, sync_function: nil, outbound_connections: []  do

  @moduledoc """
  Metadata for the Neuron node:

	* `sync_function` - this function is called when the sensor receives a sync message.  
                      the return value is expected to be an input vector, and is forwarded
                      to all outbound connections.
	* `outbound_connections` - a list of pid's of output nodes this sensor should send output to

  """
	use Neurlang

	record_type sync_function: (fun(any) -> [number])
	record_type outbound_connections: [pid]

	@spec start_node(Sensor.options | Sensor.t) :: pid
	def start_node(keywords) when is_list(keywords) do
		start_node(new(keywords))
	end
	def start_node(sensor) do
		{:ok, pid} = NodeProcess.start_link(sensor)
		pid
	end

end

defimpl Accumulator, for: Sensor do

	def create_barrier(node) do 
		node
	end

	def update_barrier_state(node, {_from_pid, _input_value}) do
		# Sensors are not expected to receive inputs, so this is ignored.  Use sync_function instead
		node
	end

	def is_barrier_satisfied?(_node) do
		# Sensors are not expected to receive inputs, so this is ignored.  Use sync_function instead
		true
	end

	def compute_output(_node) do
		# Sensors are not expected to receive inputs, so this is ignored.  Use sync_function instead
		[]
	end

	def propagate_output(node, output) do
		message = {self(), :forward, output}
		Enum.each node.outbound_connections(), fn(node_pid) -> 
																								node_pid <- message 
																						end
	end

	def sync(node) do
		f = node.sync_function()
		f.( )
	end

end

defimpl ConnectedNode, for: Sensor do
	import Neurlang, only: [validate_pid: 1]

	def add_inbound_connection(node, _from_node_pid) do
		# Sensors are not expected to have inbound connections, so this is ignored.  Use sync_function instead
		node
	end

	def add_inbound_connection(node, _from_node_pid, _weights) do
		# Sensors are not expected to have inbound connections, so this is ignored.  Use sync_function instead
		node
	end
	
	def add_outbound_connection(node, to_node_pid) do
		validate_pid(to_node_pid)
		node.outbound_connections([to_node_pid | node.outbound_connections()])
	end

end
