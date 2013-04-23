defprotocol Neurlang.ConnectedNode do
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.Senor, as: Sensor
	alias Neurlang.Actuator, as: Actuator
  
	@doc """
  Add an inbound connection from from_node into this node, with the given weights
  """

	def pid( node )

	def add_inbound_connection( node, from_node, weights )

	def add_inbound_connection( node, from_node )
	
	def add_outbound_connection( node, to_node )


end