defprotocol Neurlang.Accumulator do
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.Senor, as: Sensor
	alias Neurlang.Actuator, as: Actuator
  
	@doc """
  Update the barrier in the node to reflect the fact that we've received
  an input from this pid, and return the new node
  """
	def update_barrier_state(node, {from_pid, input_value}) 

	@doc """
	The barrier is satisfied when there is a pid key in the barrier for every single pid
	in the inbound_connections array
  """
	def is_barrier_satisfied?(node) 

	@doc """
  Compute the output after all inputs have been accumulated.
  """
	def compute_output(node)

	@doc """
  Propagate the output message to all outbound connnections
  """
	def propagate_output(node, output) 
	

end