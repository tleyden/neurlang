defmodule Neurlang.Connector do

	use Neurlang
	alias Neurlang, as: N

	@doc """
  Create a bi-directional connection from node -> to_node, using the given 
  weights to weight the connections
  """
  @spec connect(Keywords.t) :: {N.neurlang_node, N.neurlang_node}
	def connect(from: node, to: to_node, weights: weights) do
		node = NodeProcess.add_outbound_connection(node, to_node) 
		to_node = NodeProcess.add_inbound_connection(to_node, node, weights)
		{node, to_node}
	end

	@doc """
  Create a bi-directional connection from node -> to_node, unweighted connection
  """
	def connect(from: node, to: to_node) do
		node = NodeProcess.add_outbound_connection(node, to_node) 
		to_node = NodeProcess.add_inbound_connection(to_node, node)
		{node, to_node}
	end

end