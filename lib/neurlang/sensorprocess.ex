
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
		IO.puts "sensor pid: #{inspect(pid)}"
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

	@doc false
	def handle_call( {:add_outbound_connection, neuron}, _from, state) do
		state = state.outbound_connections( [ neuron.pid() | state.outbound_connections() ] )
		{ :reply, state, state }
	end

	@doc """
  Cause this sensor to sense the environment and send outputs
  """
	def sync(sensor) do
		:gen_server.cast(sensor.pid(), :sync)
	end

	@doc """
	Add an outbound connection from this sensor to given neuron
	"""
	def add_outbound_connection(sensor, neuron) do
		IO.puts "add outbound connection called with: #{inspect(sensor)}"
		sensor = :gen_server.call(sensor.pid(), {:add_outbound_connection, neuron} )
		sensor
	end

	defp send_random_output(state) do
		IO.puts "send_random_output.  state: #{inspect(state)}"
		output_vector = random_vector( state.output_vector_length() )
		message = { self(), :forward, output_vector }
		Enum.each state.outbound_connections(), fn(pid) -> 
																								pid <- message
																						end
	end

	defp random_vector(length) do
		IO.puts "length: |#{length}|"
		random_vector(length, [])
	end

	defp random_vector(0, acc) do
		acc
	end

	defp random_vector(length, acc) do
		random_vector(length-1, [ :random.uniform() | acc ])
	end


end
