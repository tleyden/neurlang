
defmodule Neurlang.ActuatorProcess do

	@moduledoc """
  This wraps an actuator and provides functionality to communicate with other nodes
  """
	use GenServer.Behaviour
	alias Neurlang.Actuator, as: Actuator

	@doc """
  Start the process.

	* `actuator` is an Actuator record

  """
	def start_link(state) do
		:gen_server.start_link(__MODULE__, state, [])	
	end

	@doc false
	def init(state) do
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
	Get the output from the actuator.

  * `actuator` - the actuator record.

  * `timeout_milliseconds` - will block for up to this amount waiting for barrier to fill

  Returns a tuple {:ok, output_vector} or {:error, message} if the barrier
  is not full within the timeout.
  """
	def get_output(_actuator, _timeout_milliseconds) do
		# TODO - not sure how to do this yet.. needs to wait for 
    # actuator to get all input
		30
	end

	defp handle_input({from_pid, input_value}, state) do
		"""
    Handle a new incoming input value from another node in the neural net and
    send output to all connected output nodes
    """
		state = update_barrier_state(state, {from_pid, input_value})
		{ :noreply, state }	
	end

	defp update_barrier_state(state, {from_pid, input_value}) do
   	"""
    Update the barrier in the state to reflect the fact that we've received
    an input from this pid, and return the new state
    """
		state.barrier( Dict.put(state.barrier(), from_pid, input_value) )
	end


end
