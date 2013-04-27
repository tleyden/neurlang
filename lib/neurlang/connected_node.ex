defprotocol Neurlang.ConnectedNode do

  alias Neurlang, as: N

	@doc """
  Add an inbound connection from from_node into this node, with the given weights
  """
  @spec add_inbound_connection(N.neurlang_node, pid, list(number)) :: N.neurlang_node
	def add_inbound_connection(node, from_node_pid, weights)

	@doc """
  Add an inbound connection from from_node into this node, with no weights
  """
  @spec add_inbound_connection(N.neurlang_node, pid) :: N.neurlang_node
	def add_inbound_connection(node, from_node_pid)
	
	@doc """
  Add outbound connection from this node to to_node
  """
  @spec add_outbound_connection(N.neurlang_node, pid) :: N.neurlang_node
	def add_outbound_connection(node, to_node_pid)

end