defmodule Neurlang.Connector do

	use Neurlang
	alias Neurlang, as: N

	@doc """
  Create a bi-directional connection from node -> to_node, using the given 
  weights to weight the connections
  """
  @spec connect(Keywords.t) :: :ok
	def connect(from: node_pid, to: to_node_pid, weights: weights) do
		NodeProcess.add_outbound_connection(node_pid, to_node_pid) 
		NodeProcess.add_inbound_connection(to_node_pid, node_pid, weights)
		:ok
	end

	def connect(from: node_pid, to: to_node_pid) do
		NodeProcess.add_outbound_connection(node_pid, to_node_pid) 
		NodeProcess.add_inbound_connection(to_node_pid, node_pid)
	end

end