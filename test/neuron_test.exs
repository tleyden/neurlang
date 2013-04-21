Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronTest do
  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.Sensor, as: Sensor
	alias Neurlang.Actuator, as: Actuator
	alias Neurlang.ConnectedNode, as: ConnectedNode

  test "connected node: add inbound connection" do
		
		sensor = Sensor.new(pid: :fakepid)
		neuron = Neuron.new()
		neuron = ConnectedNode.add_inbound_connection( neuron, sensor, [] )
		neuron_inbound = neuron.inbound_connections()
		{ node_pid, weights } = Enum.at! neuron_inbound, 0
		assert node_pid == :fakepid
		assert weights == []

	end

  test "connected node: add oubound connection" do
		
		actuator = Actuator.new(pid: :actuatorpid)
		neuron = Neuron.new(pid: :neuronpid)
		neuron = ConnectedNode.add_outbound_connection( neuron, actuator )
		neuron_outbound = neuron.outbound_connections()
		node_pid = Enum.at! neuron_outbound, 0
		assert node_pid == :actuatorpid

	end

	
end
