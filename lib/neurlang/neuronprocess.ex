
defmodule Neurlang.NeuronProcess do

	@moduledoc """
  This wraps a neuron and provides functionality to communicate with other nodes (sensors, neurons, actuators).  
  """
	use GenServer.Behaviour
	alias Neurlang.Neuron, as: Neuron

	@doc """
  Start the process

  * `state` - a Neuron record
  """
	def start_link(state) do
		:gen_server.start_link(__MODULE__, state, [])	
	end

	@doc false
	def init(state) do
		state = state.barrier(HashDict.new)
		{ :ok, state }
	end

	@doc false
	def handle_info({from_pid, input_value}, state) do
		handle_input({from_pid, input_value}, state)
		{ :noreply, state }
	end

	@doc false
	def handle_cast({from_pid, input_value}, state) do
		handle_input({from_pid, input_value}, state)
	end

	@doc """
  Wrap gen_server cast to provide more descriptive api
  """
	def send_input(neuronprocess_pid, args) do
		:gen_server.cast(neuronprocess_pid, args)
	end

	defp handle_input({from_pid, input_value}, state) do
		"""
    Handle a new incoming input value from another node in the neural net and
    send output to all connected output nodes
    """
		state = update_barrier_state(state, {from_pid, input_value})

		if is_barrier_satisfied(state) do
			inputs = get_inputs(state)
			output_value = NeuronMethod.compute_output(state, inputs)
			outbound_message = {:output, output_value}			
			Enum.each(state.output_nodes(), 
												fn(node) -> 
														node <- outbound_message 
												end)
			state = state.barrier(HashDict.new)
		end

		{ :noreply, state }
	end

	defp update_barrier_state(state, {from_pid, input_value}) do
		"""
    Update the barrier in the state to reflect the fact that we've received
    an input from this pid, and return the new state
    """
		state.barrier( Dict.put(state.barrier(), from_pid, input_value) )
	end

	defp get_inputs(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		"""
    Get the inputs that will be fed into neuron, which are stored in the now-full barrier.
		They must be in the same order as the keys of the input_nodes.
		"""
		lc {input_node_pid, _weights} inlist inbound_connections, do: barrier[input_node_pid]
	end

	defp is_barrier_satisfied(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		"""
		The barrier is satisfied when there is a pid key in the barrier for every single pid
		in the state.input_nodes array
    """
		inbound_connections_accounted = Enum.filter(inbound_connections, fn({pid, _weights}) -> HashDict.has_key?(barrier, pid) end)
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end

end
