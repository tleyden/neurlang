defmodule Neurlang.NodeHelper do
	@moduledoc """
  Functionality of the nodes (neuron, sensor, actuators) that is unrelated to process/communication/otp.
  """
	alias Neurlang.Neuron, as: Neuron

	def update_barrier_state(state, {from_pid, input_value}) do
		"""
    Update the barrier in the state to reflect the fact that we've received
    an input from this pid, and return the new state
    """
		state.barrier( Dict.put(state.barrier(), from_pid, input_value) )
	end

	def is_barrier_satisfied(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		"""
		The barrier is satisfied when there is a pid key in the barrier for every single pid
		in the state.input_nodes array
    """
		inbound_connections_accounted = Enum.filter(inbound_connections, fn({pid, _weights}) -> HashDict.has_key?(barrier, pid) end)
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end



end