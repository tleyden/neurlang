defmodule Neurlang.Connector do

	@moduledoc """
  Connect nodes together to form the network
  """

	@doc """
  Connect these two nodes together, with the given weight vector

  * `from_node` the record representing the source of the connection, eg a Sensor record

  * `to_node` the record representing the target of the connection, eg a Neuron record

  * `weight_vector` a list which contains the weight vector to be used when weighting 
                    input vectors from from_node
  """
	def connect_weighted(from_node, to_node, weight_vector) do
		
		# modify from_node to add to_node's pid in its outbound_connections
		new_outbound_connections = [to_node.pid() | from_node.outbound_connections() ]
		from_node = from_node.outbound_connections( new_outbound_connections )
		
		# modify to_node to add from_node's pid and weight vector to inbound_connections
		new_inbound_connections = [ {from_node.pid(), weight_vector} | to_node.inbound_connections() ]  
		to_node = to_node.inbound_connections( new_inbound_connections )

		{from_node, to_node}

	end

	@doc """
  Connect these two nodes together

  * `from_node` the record representing the source of the connection, eg a Sensor record

  * `to_node` the record representing the target of the connection, eg a Neuron record
  """
	def connect(from_node, to_node) do
		
		# modify from_node to add to_node's pid in its outbound_connections
		new_outbound_connections = [to_node.pid() | from_node.outbound_connections() ]
		from_node = from_node.outbound_connections( new_outbound_connections )
		
		# modify to_node to add from_node's pid and weight vector to inbound_connections
		new_inbound_connections = [ from_node.pid() | to_node.inbound_connections() ]  
		to_node = to_node.inbound_connections( new_inbound_connections )

		{from_node, to_node}

	end


end