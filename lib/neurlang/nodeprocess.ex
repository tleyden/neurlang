
defmodule Neurlang.NodeProcess do

	@moduledoc """
  This wraps a neuron and provides functionality to communicate with other nodes (sensors, neurons, actuators).  
  """
	use GenServer.Behaviour
	alias Neurlang.Accumulator, as: Accumulator
	alias Neurlang.ConnectedNode, as: ConnectedNode

	## API

	@doc """
  Start the process

  * `state` - a Neuron, Actuator, or Sensor record
  """
	def start_link(state) do
		{:ok, pid} = :gen_server.start_link(__MODULE__, state, [])	
		state.pid(pid)
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

	## Private

	defp handle_input({from_pid, input_value}, state) do
		"""
    Handle a new incoming input value from another node in the neural net and
    send output to all connected output nodes
    """
		state = Accumulator.update_barrier_state(state, {from_pid, input_value})

		if Accumulator.is_barrier_satisfied?(state) do
			output = Accumulator.compute_output( state )
			Accumulator.propagate_output( state, output )
			state = state.barrier(HashDict.new)
		end

		state
	end
	
	## OTP 

	@doc false
	def init( state ) do
		state = state.barrier( HashDict.new )
		state = state.pid( self() ) 
		{ :ok, state }
	end

	@doc false
	def handle_info({from_pid, :forward, input_value}, state) do
		state = handle_input({from_pid, input_value}, state)
		{ :noreply, state }
	end

	@doc false
	def handle_cast({from_pid, input_value}, state) do
		state = handle_input({from_pid, input_value}, state)
		{ :noreply, state }
	end

	@doc false
	def handle_call( {:add_inbound_connection, { node, weights }}, _from, state) do
		state = ConnectedNode.add_inbound_connection( state, node, weights )
		{ :reply, state, state }
	end

	@doc false
	def handle_call( {:add_inbound_connection, node}, _from, state) do
		state = ConnectedNode.add_inbound_connection( state, node )
		{ :reply, state, state }
	end

	@doc false
	def handle_call( {:add_outbound_connection, node}, _from, state) do
		state = ConnectedNode.add_outbound_connection( state, node )
		{ :reply, state, state }
	end


end
