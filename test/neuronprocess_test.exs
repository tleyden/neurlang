Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronProcessTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.Sensor, as: Sensor
	alias Neurlang.SensorProcess, as: SensorProcess
	alias Neurlang.Actuator, as: Actuator
	alias Neurlang.ActuatorProcess, as: ActuatorProcess
	alias Neurlang.Mock.MockNode, as: MockNode
	alias Neurlang.Mock.MockNodeProcess, as: MockNodeProcess
	alias Neurlang.Connector, as: Connector
	alias Neurlang.TestHelper, as: TestHelper

	test "new v2 connected neuron" do

		neuron1 = Neuron.new(id: make_ref(), bias: bias(10), activation_function: function(:summation, 3))
		{ :ok, neuron1_pid } = NeuronProcess.start_link(neuron1)
		neuron1 = neuron1.pid(neuron1_pid)

		sensor = Sensor.new(id: make_ref(), output_vector_length: nil)
		actuator = Actuator.new(id: make_ref())
		
		{ :ok, sensor_pid } = SensorProcess.start_link(sensor)
		{ :ok, actuator_pid } = ActuatorProcess.start_link(actuator)

		sensor = sensor.pid(sensor_pid)
		actuator = actuator.pid(actuator_pid)
	
		{sensor, _neuron1} = Connector.connect_weighted(sensor, neuron1, weight([20]))  
		{_neuron1, actuator} = Connector.connect(neuron1, actuator)

		SensorProcess.sync(sensor)
		timeout_milliseconds = 500
		received = ActuatorProcess.get_output(actuator, timeout_milliseconds)
		assert(received == 30)

	end

	def summation(inputs, weights, bias) do
		TestHelper.summation(inputs, weights, bias)
	end


	def receive_actuator_output() do
		:timeout
	end

	def weight(x) do
		x
	end

	def bias(x) do
		x
	end

end