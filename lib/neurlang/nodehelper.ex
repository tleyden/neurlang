defmodule Neurlang.NodeHelper do
	@moduledoc """
  Functionality of the nodes (neuron, sensor, actuators) that is unrelated to process/communication/otp.
  """
	alias Neurlang.Neuron, as: Neuron

	def update_barrier_state(node, {from_pid, input_value}) do
		"""
    Update the barrier in the node to reflect the fact that we've received
    an input from this pid, and return the new node
    """
		IO.puts "upate_barrier_state: |#{inspect(input_value)}|"
		node.barrier( Dict.put(node.barrier(), from_pid, input_value) )
	end




end