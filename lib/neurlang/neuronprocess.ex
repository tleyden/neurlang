
defmodule Neurlang.NeuronProcess do

	@moduledoc """
  This wraps a neuron and provides functionality to communicate with other nodes (sensors, neurons, actuators).  
  """
	use GenServer.Behaviour
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.Sensor, as: Sensor
	alias Neurlang.NeuronHelper, as: NeuronHelper
	alias Neurlang.Barrier, as: Barrier
	alias Neurlang.ConnectedNode, as: ConnectedNode

	## API

	@doc """
  Start the process

  * `state` - a Neuron record
  """
	def start_link(state) do
		{:ok, pid} = :gen_server.start_link(__MODULE__, state, [])	
		state.pid(pid)
	end

	@doc """
	Add an inbound connection to this neuron from node (sensor | neuron)
	"""
	def add_inbound_connection( neuron, node, weights ) do
		message = { :add_inbound_connection, { node, weights } }
		:gen_server.call( neuron.pid(), message )
	end

	@doc """
	Add an outbound connection from this neuron to given node
	"""
	def add_outbound_connection( Neuron[pid: pid], node) do
		:gen_server.call(pid, {:add_outbound_connection, node} )
	end

	## Private

	defp handle_input({from_pid, input_value}, state) do
		"""
    Handle a new incoming input value from another node in the neural net and
    send output to all connected output nodes
    """
		IO.puts "neuron.handle_input called with: #{inspect(input_value)}"
		state = Barrier.update_barrier_state(state, {from_pid, input_value})

		if Barrier.is_barrier_satisfied?(state) do
			IO.puts "neuron barrier satisfied, sending outbound messages"
			output = [ NeuronHelper.compute_output( state ) ]
			message = { self(), :forward, output }
			Enum.each state.outbound_connections(), fn(node) -> 
																									node <- message 
																							end
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
		IO.puts "neuron handle_info called"
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
	def handle_call( {:add_outbound_connection, node}, _from, state) do
		state = ConnectedNode.add_outbound_connection( state, node )
		{ :reply, state, state }
	end


end
