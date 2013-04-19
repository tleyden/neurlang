
defmodule Neurlang.ActuatorProcess do

	@moduledoc """
  This wraps an actuator and provides functionality to communicate with other nodes
  """
	use GenServer.Behaviour
	alias Neurlang.Actuator, as: Actuator

	## API 

	@doc """
  Start the process.

	* `actuator` is an Actuator record

  """
	def start_link(state) do
		{:ok, pid} = :gen_server.start_link(__MODULE__, state, [])	
		state.pid(pid)
	end

	@doc """
	Get the current state (actuator record)
  """
	def get_current_state(Actuator[pid: pid]) do
		:gen_server.call( pid, :get_current_state )
	end

	@doc """
	Add an inbound connection to this actuator from node (sensor | neuron)
	"""
	def add_inbound_connection( Actuator[pid: pid], node ) do
		:gen_server.call( pid, { :add_inbound_connection, node } )
	end

	@doc """
	Add an outbound connection from this neuron to given node
	"""
	def add_outbound_connection( Actuator[pid: pid], node) do
		:gen_server.call(pid, {:add_outbound_connection, node} )
	end

	## Private

	defp handle_input({from_pid, input_value}, state) do
		"""
    Handle a new incoming input value from another node in the neural net and
    send output to all connected output nodes
    """
		state = update_barrier_state(state, {from_pid, input_value})
		IO.puts "post update_barrier_state: state: #{inspect(state)}"
		{ :noreply, state }	
	end

	defp update_barrier_state(state, {from_pid, input_value}) do
   	"""
    Update the barrier in the state to reflect the fact that we've received
    an input from this pid, and return the new state
    """
		IO.puts "update_barrier_state: state: #{inspect(state)} from_pid: #{inspect(from_pid)} input_value: #{inspect(input_value)}"
		state.barrier( Dict.put(state.barrier(), from_pid, input_value) )
	end

	## OTP

	@doc false
	def init(state) do
		state = state.pid( self() ) 
		{ :ok, state }
	end

	@doc false
	def handle_info({from_pid, :forward, input_value}, state) do
		IO.puts "actuator handle_info called with: from_pid: #{inspect(from_pid)} input_value: #{inspect(input_value)}"
		handle_input({from_pid, input_value}, state)
		{ :noreply, state }
	end

	@doc false
	def handle_cast({from_pid, input_value}, state) do
		handle_input({from_pid, input_value}, state)
	end

	@doc false
	def handle_call( :get_current_state, _from, state) do
		{ :reply, state, state }
	end

	@doc false
	def handle_call( {:add_inbound_connection, node}, _from, state) do
		state = state.inbound_connections( [ node.pid() | state.inbound_connections() ] )
		{ :reply, state, state }
	end

	@doc false
	def handle_call( {:add_outbound_connection, node}, _from, state) do
		IO.puts "handle_call, node: #{inspect(node)} node.pid: #{inspect(node.pid())}"
		state = state.outbound_connections( [ node.pid() | state.outbound_connections() ] )
		{ :reply, state, state }
	end



end
