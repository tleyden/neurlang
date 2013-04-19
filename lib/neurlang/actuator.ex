

defrecord Neurlang.Actuator, id: nil, pid: nil, inbound_connections: [], outbound_connections: [], 
										         barrier: HashDict.new do

  @moduledoc """
  Metadata for the Neuron node:

  * `id` - a unique id gotten from calling make_ref()

	* `pid` - the process id

	* `inbound_connections` - a list of pid's of neurons nodes this actuator should expect to receive input from

	* `outbound_connections` - a list of pid's of output nodes this actuator process should send output to. 
                             added for testing purposes, but could have other uses as well.

	* `barrier` - used to wait until receiving inputs from all connected input nodes before sending output

  """

end


