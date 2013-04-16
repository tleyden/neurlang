
defmodule Neurlang.NeuronProcess do
	@moduledoc """
  This wraps a neuron and provides functionality to handle communication with other processes.  
  """
	use GenServer.Behaviour
	alias Neurlang.Neuron, as: Neuron

	@doc """
  Start the process
  """
	def start_link(state) do
		:gen_server.start_link({:local, __MODULE__}, __MODULE__, state, [])	
	end

	@doc false
	def init(state) do
		IO.puts "neuronprocess.init called"
		{ :ok, state }
	end

	@doc false
	def handle_info(msg, state) do
		IO.puts "handle_info called with unexpected message: #{inspect(msg)}"
		{ :noreply, state }
	end

	@doc false
	def handle_cast({from_pid, input_value}, state) do
		IO.puts "handle_cast called by: #{inspect(from_pid)} state: #{inspect(state)}"
		## TODO: this is wrong.  it needs to wait until receiving inputs from all inbound neurons
    ## before calling activation function and sending output
		{parameters, connected_nodes} = state
		inputs = [input_value]
		output = Neuron.process_input_vector(parameters, inputs)
		message = {:output, output}
		IO.puts "sending #{inspect(message)} to #{inspect(from_pid)}"
		from_pid <- message
		{ :noreply, state }
	end

	@doc """
  Wrap gen_server handle_cast to provide slightly cleaner api
  """
	def process_input(neuronprocess_pid, args) do
		:gen_server.cast(neuronprocess_pid, args)
	end


end
