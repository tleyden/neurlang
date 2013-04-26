Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronTest do
  use ExUnit.Case
  use Neurlang

	test "compute output" do 

		bias = 50
		activation_function = fn x -> x * 2 end
		input_vector1 = [10, 10]
		weight_vector1 = [5, 5]
		input_vector2 = [20, 20]
		weight_vector2 = [2, 2]

		sensor1 = Sensor.new(pid: :sensor1)
		sensor2 = Sensor.new(pid: :sensor2)

		neuron = Neuron.new(bias: bias, activation_function: activation_function)
		neuron = ConnectedNode.add_inbound_connection( neuron, sensor1, weight_vector1 )
		neuron = ConnectedNode.add_inbound_connection( neuron, sensor2, weight_vector2 )

		barrier = HashDict.new
		barrier = Dict.put( barrier, :sensor1, input_vector1)
		barrier = Dict.put( barrier, :sensor2, input_vector2)
	
		neuron = neuron.barrier( barrier )

		result = Accumulator.compute_output(neuron)
		assert result == [ 460 ]

	end


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
