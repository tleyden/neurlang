
defmodule Neurlang.NodeProcess do

	@moduledoc """
  This provides the _actor_ functionality of neural elements in neurlang.  

  Each type of element (sensor, neuron, actuator) can be _wrapped_ by this nodeprocess, 
  which allows it to communicate with other nodes (sensors, neurons, actuators) using 
  async message passing.

	The gen_server state is represented by the appropriate node record (sensor, neuron, actuator).
  """
	use GenServer.Behaviour
	use Neurlang
  alias Neurlang, as: N

	## API

	@doc """
  Start the process

  * `node` - a Neuron, Actuator, or Sensor
  """
	@spec start_link(N.neuro_node) :: {:ok | :error, pid}
	def start_link(node) do
		:gen_server.start_link(__MODULE__, node, [])	
	end

	@doc """
	Add an inbound connection to this neuron from node (sensor | neuron)
	"""
	@spec add_inbound_connection(N.neurlang_node, N.neurlang_node, list(number)) :: N.neurlang_node
	def add_inbound_connection(node, from_node, weights) do
		message = {:add_inbound_connection, {from_node, weights}}
		:gen_server.call(ConnectedNode.pid(node), message)
	end

	@doc """
	Add an inbound connection to this actuator from node (sensor | neuron)
	"""
  @spec add_inbound_connection(N.neurlang_node, N.neurlang_node) :: N.neurlang_node
	def add_inbound_connection(node, from_node) do
		:gen_server.call(ConnectedNode.pid(node), {:add_inbound_connection, from_node})
	end

	@doc """
	Add an outbound connection from this neuron to given node
	"""
  @spec add_outbound_connection(N.neurlang_node, N.neurlang_node) :: N.neurlang_node
	def add_outbound_connection(node, to_node) do
		:gen_server.call(ConnectedNode.pid( node ), {:add_outbound_connection, to_node})
	end

	@doc """
  Cause this sensor to sense the environment and send outputs.  
  """
  @spec sync(N.neurlang_node) :: N.neurlang_node
	def sync(node) do
		:gen_server.cast(ConnectedNode.pid( node ), :sync)
	end

	## Private
  @spec handle_input({pid, list(number)}, N.neurlang_node) :: N.neurlang_node
	defp handle_input({from_pid, input_value}, node) do
		"""
    Handle a new incoming input value from another node in the neural net and
    send output to all connected output nodes
    """
		node = Accumulator.update_barrier_state(node, {from_pid, input_value})
		if Accumulator.is_barrier_satisfied?(node) do
			output = Accumulator.compute_output(node)
			Accumulator.propagate_output(node, output)
			node = Accumulator.create_barrier(node)
		end
		node
	end
	
	## OTP 

	@doc false
  @spec init(N.neurlang_node) :: {:ok, N.neurlang_node}
	def init(node) do
		node = Accumulator.create_barrier(node)
		node = node.pid(self()) 
		{:ok, node}
	end

	@doc false
  @spec handle_info(N.node_message, N.neurlang_node) :: {:noreply, N.neurlang_node}
	def handle_info({from_pid, :forward, input_value}, node) do
		node = handle_input({from_pid, input_value}, node)
		{:noreply, node}
	end

	@doc false
  @spec handle_cast(:sync | {pid, list(number)}, N.neurlang_node) :: {:noreply, N.neurlang_node}
	def handle_cast({from_pid, input_value}, node) do
		node = handle_input({from_pid, input_value}, node)
		{:noreply, node}
	end

	@doc false
	def handle_cast(:sync, node) do
		output = Accumulator.sync(node)
		Accumulator.propagate_output(node, output)
		{:noreply, node}
	end

	@doc false
	@spec handle_call(N.handle_call_msg, {pid,any}, N.neurlang_node) :: {:reply, N.neurlang_node, N.neurlang_node}
	def handle_call({:add_inbound_connection, {from_node, weights}}, _from_pid, node) do
		node = ConnectedNode.add_inbound_connection(node, from_node, weights)
		{:reply, node, node}
	end

	@doc false
	def handle_call({:add_inbound_connection, from_node}, _from_pid, node) do
		node = ConnectedNode.add_inbound_connection(node, from_node)
		{:reply, node, node}
	end

	@doc false
	def handle_call({:add_outbound_connection, to_node}, _from_pid, node) do
		node = ConnectedNode.add_outbound_connection(node, to_node)
		{:reply, node, node}
	end

end
