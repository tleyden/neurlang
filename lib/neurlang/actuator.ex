
alias Neurlang.Barrier, as: Barrier
alias Neurlang.Actuator, as: Actuator

defrecord Neurlang.Actuator, id: nil, pid: nil, inbound_connections: [], outbound_connections: [], 
										         barrier: HashDict.new do

  @moduledoc """
  Metadata for the Actuator node:

  * `id` - a unique id gotten from calling make_ref()

	* `pid` - the process id

	* `inbound_connections` - a list of pid's of neurons nodes this actuator should expect to receive input from

	* `outbound_connections` - a list of pid's of output nodes this actuator process should send output to. 
                             added for testing purposes, but could have other uses as well.

	* `barrier` - used to wait until receiving inputs from all connected input nodes before sending output

  """

end

defimpl Barrier, for: Actuator do

	def update_barrier_state(node, {from_pid, input_value}) do
		node.barrier( Dict.put(node.barrier(), from_pid, input_value) )
	end

	def is_barrier_satisfied?(Actuator[inbound_connections: inbound_connections, barrier: barrier]) do
		inbound_connections_accounted = Enum.filter(inbound_connections, fn(pid) -> HashDict.has_key?(barrier, pid) end)
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end



end