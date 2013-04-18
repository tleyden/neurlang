defmodule Neurlang.Mock.MockNodeProcess do

	@moduledoc """
  Mock Process that can act like a node (neuron, sensor, etc) for testing purposes
  """
	use GenServer.Behaviour

	@doc """
  Start the process
  """
	def start_link(state) do
		:gen_server.start_link(__MODULE__, state, [])	
	end

	@doc false
	def init(state) do
		{ :ok, state }
	end

	@doc false
	def handle_info(msg, state) do
		IO.puts "MockNodeProcess.handle_info called with message: #{inspect(msg)}"
		{ :noreply, state }
	end

	def send_input(mocknode, args) do
		:gen_server.cast(mocknode.pid(), args)
	end


end