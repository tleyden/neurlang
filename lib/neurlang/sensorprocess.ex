
defmodule Neurlang.SensorProcess do

	@moduledoc """
  This wraps a sensor and provides functionality to communicate with other nodes
  """
	use GenServer.Behaviour
	alias Neurlang.Sensor, as: Sensor

	@doc """
  Start the process.

	* `state` is a Sensor record

  """
	def start_link(state) do
		{:ok, pid} = :gen_server.start_link(__MODULE__, state, [])	
		state.pid(pid)
	end

	@doc false
	def init(state) do
		state = state.pid( self() ) 
		{ :ok, state }
	end

	@doc false
	def handle_info(msg, state) do
		IO.puts "SensorProcess recieved unexpected handle_info message: #{(msg)}"
		{ :noreply, state }
	end

	@doc false
	def handle_cast(:sync, state) do
		send_random_output(state)
		{ :noreply, state }
	end

	@doc """
  Cause this sensor to sense the environment and send outputs
  """
	def sync(sensor) do
		:gen_server.cast(sensor.pid(), :sync)
	end

	defp send_random_output(state) do
		IO.puts "send_random_output.  state: #{inspect(state)}"
		output_vector = random_vector( state.output_vector_length() )
		Enum.each state.outbound_connections(), fn(pid) -> 
																								pid <- output_vector
																						end
	end

	defp random_vector(length) do
		IO.puts "length: |#{length}|"
		random_vector(length, [])
	end

	defp random_vector(length, acc) do
		random_vector(length-1, [ :random.uniform() | acc ])
	end

	defp random_vector(0, acc) do
		acc
	end

end
