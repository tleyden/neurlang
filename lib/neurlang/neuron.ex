
alias Neurlang.ConnectedNode, as: ConnectedNode
alias Neurlang.Accumulator, as: Accumulator
alias Neurlang.Neuron, as: Neuron

defrecord Neurlang.Neuron, id: nil, pid: nil, activation_function: nil, bias: nil, 
										       inbound_connections: [], outbound_connections: [], barrier: HashDict.new do

  @moduledoc """
  Metadata for the Neuron node:

  * `id` - a unique id gotten from calling make_ref()

	* `pid` - the process id

  * `activation_function` - a function which will be used to calculate the output, eg, a sigmoid funtion

  * `bias` - after dot product of input vector and weight vector is taken, a scalar bias value is added

	* `inbound_connections` - a list of {pid, weight_vector} tuples representing inbound connections

	* `outbound_connections` - a list of pid's of output nodes this neuron process should send output to

	* `barrier` - used to wait until receiving inputs from all connected input nodes before sending output

  """

	use Neurlang

	record_type id: reference
	record_type pid: pid
	record_type activation_function: (fun(number) -> number)
	record_type bias: number
	record_type inbound_connections: [{pid, list}]
	record_type outbound_connections: [pid]
	record_type barrier: Dict

	@spec start_node(Neuron.options) :: Neuron.t
	def start_node(keywords) do
		neuron = Neuron.new(keywords)
		NodeProcess.start_link(neuron)
	end


end

defimpl Accumulator, for: Neuron do
	alias Neurlang.MathUtil, as: MathUtil
	import MathUtil, only: [dot_product: 2]

	def create_barrier(node) do
		node.barrier(HashDict.new)
	end

	def update_barrier_state(Neuron[] = node, {from_pid, input_value}) do
		node.barrier(Dict.put(node.barrier(), from_pid, input_value))
	end

	def is_barrier_satisfied?(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		inbound_connections_accounted = Enum.filter(inbound_connections, fn({pid, _weights}) -> 
																																				 HashDict.has_key?(barrier, pid) 
																																		 end)
		length(inbound_connections_accounted) == length(inbound_connections)																					
	end

	@doc """
	Compute the output for this neuron based on its parameters (bias, activation function)
	and the inputs/weights tuples stored in the barrier structure, which is presumed to
  be full with inputs from all inbound nodes.
	"""
	def compute_output(neuron) do
		Neuron[activation_function: activation_function, bias: bias] = neuron
		weighted_inputs = get_weighted_inputs(neuron)
		scalar_output = compute_output(weighted_inputs, bias, activation_function)
		[ scalar_output ]
	end

	def propagate_output(node, output) do
		message = { node.pid(), :forward, output }
		Enum.each node.outbound_connections(), fn(node) -> 
																								node <- message 
																						end
	end

	def sync(node) do
		if node, do: throw "Neurons do not have sync functionality yet"
		node
	end

	@doc false
	defp compute_output(weighted_inputs, bias, activation_function) do
		reduce_function = fn({inputs, weights}, acc) -> 
													dot_product(inputs, weights) + acc 
											end
		output = Enum.reduce weighted_inputs, 0, reduce_function 
		output = output + bias
		activation_function.(output)
	end

	@doc false
	defp get_weighted_inputs(Neuron[inbound_connections: inbound_connections, barrier: barrier]) do
		"""
    Get the inputs that will be fed into neuron, which are stored in the now-full barrier.
    Returns a list of the form [{input_vector,weight_vector}, ...]
		"""
		lc {input_node_pid, weights} inlist inbound_connections do
			inputs = barrier[input_node_pid]
			if length(inputs) != length(weights) do 
				throw "length of inputs #{inspect(inputs)} != length of weights #{inspect(weights)}"
			end
			{ inputs, weights } 
		end

	end


end

defimpl ConnectedNode, for: Neuron do

	def pid(node) do
		node.pid()
	end

	def add_inbound_connection(node, from_node, weights) do
		inbound_connection = {from_node.pid(), weights}
		node.inbound_connections([inbound_connection | node.inbound_connections()])
	end

	def add_inbound_connection(node, _from_node) do 
		if node, do: throw "Neuron inbound connections must have weights associated with them"
		node
	end
	
	def add_outbound_connection(node, to_node) do
		node.outbound_connections([to_node.pid() | node.outbound_connections()])
	end

end