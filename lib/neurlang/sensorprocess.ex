
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
		:gen_server.start_link(__MODULE__, state, [])	
	end

	@doc false
	def init(state) do
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

	defp send_random_output(_state) do
		# TODO
		IO.puts "send_random_output"

	end


end
