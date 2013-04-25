defmodule Neurlang.Connector do
	alias Neurlang.NodeProcess, as: NodeProcess

	def connect( from: node, to: to_node, weights: weights ) do
		node = NodeProcess.add_outbound_connection( node, to_node ) 
		to_node = NodeProcess.add_inbound_connection( to_node, node, weights )
		{ node, to_node }
	end

	def connect_without_weights( from: node, to: to_node ) do
		node = NodeProcess.add_outbound_connection( node, to_node ) 
		to_node = NodeProcess.add_inbound_connection( to_node, node )
		{ node, to_node }
	end

end