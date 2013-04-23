
defmodule Neurlang.NodeProcess do

	@moduledoc """
  This provides the _actor_ functionality of neural elements in neurlang.  

  Each type of element (sensor, neuron, actuator) can be _wrapped_ by this nodeprocess, 
  which allows it to communicate with other nodes (sensors, neurons, actuators) using 
  async message passing.

	The gen_server state is represented by the appropriate node record (sensor, neuron, actuator).
  """
	use GenServer.Behaviour
	alias Neurlang.Accumulator, as: Accumulator
	alias Neurlang.ConnectedNode, as: ConnectedNode

	## API

	@doc """
  Start the process

  * `node` - a Neuron, Actuator, or Sensor
  """
	def start_link( node ) do
		{:ok, pid} = :gen_server.start_link(__MODULE__, node, [])	
		node.pid(pid)
	end

	@doc """
	Add an inbound connection to this neuron from node (sensor | neuron)
	"""
	def add_inbound_connection( node, from_node, weights ) do
		message = { :add_inbound_connection, { from_node, weights } }
		:gen_server.call( ConnectedNode.pid( node ), message )
	end

	@doc """
	Add an inbound connection to this actuator from node (sensor | neuron)
	"""
	def add_inbound_connection( node, from_node ) do
		:gen_server.call( ConnectedNode.pid( node ), { :add_inbound_connection, from_node } )
	end

	@doc """
	Add an outbound connection from this neuron to given node
	"""
	def add_outbound_connection( node, to_node) do
		:gen_server.call( ConnectedNode.pid( node ), {:add_outbound_connection, to_node} )
	end

	@doc """
  Cause this sensor to sense the environment and send outputs.  
  """
	def sync( node ) do
		:gen_server.cast( ConnectedNode.pid( node ), :sync )
	end

	## Private

	defp handle_input( {from_pid, input_value}, node ) do
		"""
    Handle a new incoming input value from another node in the neural net and
    send output to all connected output nodes
    """
		node = Accumulator.update_barrier_state( node, {from_pid, input_value} )

		if Accumulator.is_barrier_satisfied?( node ) do
			output = Accumulator.compute_output( node )
			Accumulator.propagate_output( node, output )
			node = Accumulator.create_barrier( node )
		end

		node
	end
	
	## OTP 

	@doc false
	def init( node ) do
		node = Accumulator.create_barrier( node )
		node = node.pid( self() ) 
		{ :ok, node }
	end

	@doc false
	def handle_info( {from_pid, :forward, input_value}, node ) do
		node = handle_input( {from_pid, input_value}, node )
		{ :noreply, node }
	end

	@doc false
	def handle_cast( {from_pid, input_value}, node ) do
		node = handle_input( {from_pid, input_value}, node )
		{ :noreply, node }
	end

	@doc false
	def handle_cast( :sync, node ) do
		output = Accumulator.sync( node )
		Accumulator.propagate_output( node, output )
		{ :noreply, node }
	end

	@doc false
	def handle_call( {:add_inbound_connection, { from_node, weights }}, _from_pid, node) do
		node = ConnectedNode.add_inbound_connection( node, from_node, weights )
		{ :reply, node, node }
	end

	@doc false
	def handle_call( {:add_inbound_connection, from_node}, _from_pid, node ) do
		node = ConnectedNode.add_inbound_connection( node, from_node )
		{ :reply, node, node }
	end

	@doc false
	def handle_call( {:add_outbound_connection, to_node}, _from_pid, node ) do
		node = ConnectedNode.add_outbound_connection( node, to_node )
		{ :reply, node, node }
	end



end
