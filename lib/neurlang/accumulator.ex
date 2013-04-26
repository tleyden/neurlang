defprotocol Neurlang.Accumulator do

  alias Neurlang, as: N

	@doc """
  Create / reset the barrier 
  """
  @spec create_barrier(N.neurlang_node) :: N.neurlang_node
	def create_barrier(node) 
  
	@doc """
  Update the barrier in the node to reflect the fact that we've received
  an input from this pid, and return the new node
  """
  @spec update_barrier_state(N.neurlang_node, N.barrier_state) :: N.neurlang_node
	def update_barrier_state(node, barrier) 

	@doc """
	The barrier is satisfied when there is a pid key in the barrier for every single pid
	in the inbound_connections array
  """
  @spec is_barrier_satisfied?(N.neurlang_node) :: boolean
	def is_barrier_satisfied?(node) 

	@doc """
  Compute the output after all inputs have been accumulated.
  """
  @spec compute_output(N.neurlang_node) :: list(number)
	def compute_output(node)

	@doc """
  Propagate the output message to all outbound connnections
  """
  @spec propagate_output(N.neurlang_node, list(number)) :: :ok
	def propagate_output(node, output) 

	@doc """
  Tell a sensor to gather inputs from input source and send to outbound connections.
  For other nodes, it could be used to trigger it to send current intputs to outbound
  connections even if barrier is not satisfied yet.
  """
  @spec sync(N.neurlang_node) :: N.neurlang_node
	def sync(node)
	

end