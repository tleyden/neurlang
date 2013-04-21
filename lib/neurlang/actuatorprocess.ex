
defmodule Neurlang.ActuatorProcess do

	@moduledoc """
  This wraps an actuator and provides functionality to communicate with other nodes
  """
	use GenServer.Behaviour
	alias Neurlang.Actuator, as: Actuator
	alias Neurlang.Barrier, as: Barrier

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
		state = Barrier.update_barrier_state(state, {from_pid, input_value})

		if Barrier.is_barrier_satisfied?(state) do
			received_inputs = get_received_inputs(state)
			outputs = List.flatten( received_inputs ) 
			message = { self(), :forward, outputs }
			IO.puts "state: #{inspect(state)} sending message: #{inspect(message)}"
			Enum.each state.outbound_connections(), fn(node) -> 
																									node <- message 
																							end
			state = state.barrier(HashDict.new)
		end
		state 	
	end

	defp get_received_inputs(state) do
			barrier = state.barrier()
			inbound_connections = state.inbound_connections()
			lc input_node_pid inlist inbound_connections, do: barrier[input_node_pid]
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
		state = handle_input({from_pid, input_value}, state)
		{ :noreply, state }
	end

	@doc false
	def handle_cast({from_pid, input_value}, state) do
		state = handle_input({from_pid, input_value}, state)
		{ :noreply, state }
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
