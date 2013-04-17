
defmodule Neurlang.NeuronProcess do

	@moduledoc """
  This wraps a neuron and provides functionality to communicate with other nodes (sensors, neurons, actuators).  
  """
	use GenServer.Behaviour
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcessState, as: NeuronProcessState

	@doc """
  Start the process
  """
	def start_link(state) do
		:gen_server.start_link({:local, __MODULE__}, __MODULE__, state, [])	
	end

	@doc false
	def init(state) do
		state = state.barrier(HashDict.new)
		{ :ok, state }
	end

	@doc false
	def handle_info(msg, state) do
		IO.puts "handle_info called with unexpected message: #{inspect(msg)}"
		{ :noreply, state }
	end

	@doc false
	def handle_cast({from_pid, input_value}, state) do
		handle_input({from_pid, input_value}, state)
	end

	@doc """
  Wrap gen_server cast to provide more descriptive api
  """
	def process_input(neuronprocess_pid, args) do
		:gen_server.cast(neuronprocess_pid, args)
	end

	@doc """
  Handle a new incoming input value from another node in the neural net and
  send output to all connected output nodes
  """
	defp handle_input({from_pid, input_value}, state) do

		state = update_barrier_state(state, {from_pid, input_value})

		if is_barrier_satisfied(state) do
			inputs = get_inputs(state)
			output_value = Neuron.process_input_vector(state.parameters(), inputs)  
			outbound_message = {:output, output_value}			
			Enum.each state.output_nodes(), 
												fn(node) -> 
														IO.puts "sending #{inspect(outbound_message)} to #{inspect(node)}"
														node <- outbound_message 
												end
			state = state.barrier(HashDict.new)
		else
			IO.puts("barrier not satisfied")
		end

		{ :noreply, state }
	end

	@doc """
  Update the barrier in the state to reflect the fact that we've received
  an input from this pid, and return the new state
  """
	defp update_barrier_state(state, {from_pid, input_value}) do
		barrier = Dict.put(state.barrier(), from_pid, input_value)
		state = state.barrier(barrier)
		state
	end

	@doc """
  Get the inputs that will be fed into neuron, which are stored in the now-full barrier.
  They must be in the same order as the keys of the input_nodes.
  """

	defp get_inputs(NeuronProcessState[input_nodes: input_nodes, barrier: barrier]) do
		lc input_node_pid inlist input_nodes, do: barrier[input_node_pid]
	end

	@doc """
  The barrier is satisfied when there is a pid key in the barrier for every single pid
  in the state.input_nodes array
  """
	defp is_barrier_satisfied(NeuronProcessState[input_nodes: input_nodes, barrier: barrier]) do
		input_nodes_accounted = Enum.filter(input_nodes, fn(pid) -> HashDict.has_key?(barrier, pid) end)
		length(input_nodes_accounted) == length(input_nodes)																					
	end

end
