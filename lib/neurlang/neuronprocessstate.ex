
defrecord Neurlang.NeuronProcessState, parameters: nil, input_nodes: nil, output_nodes: nil, barrier: nil do
  @moduledoc """
  The state of neuron process:

  * `parameters` - the parameters to run the underlying neuron with 

	* `input_nodes` - a list of pid's of input nodes this neuron process should expect input from

	* `output_nodes` - a list of pid's of output nodes this neuron process should send output to

	* `barrier` - used to wait until receiving inputs from all connected input nodes before sending output

  """
end
