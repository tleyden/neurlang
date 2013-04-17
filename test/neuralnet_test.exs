Code.require_file "../test_helper.exs", __FILE__

defmodule NeuralNetworkTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.TestHelper, as: TestHelper

	test "create a full neural net and feed data through it" do

		# NOTE: doesn't work yet .. just building the API

		f = function(:summation, 3)

		# create network objects
		sensor1 = Sensor.new(id: make_ref())
		sensor2 = Sensor.new(id: make_ref())
		neuron1_1 = Neuron.new(id: make_ref(), bias: bias(10), activation_function: f)
		neuron1_2 = Neuron.new(id: make_ref(), bias: bias(-10), activation_function: f)
		neuron2_1 = Neuron.new(id: make_ref(), bias: bias(-10), activation_function: f)
		actuator = Actuator.new(id: make_ref())

		# start network processes
		{ :ok, sensor1 } = SensorProcess.start_link(sensor1)
		{ :ok, sensor2 } = SensorProcess.start_link(sensor2)
		{ :ok, neuron1_1 } = NeuronProcess.start_link(neuron1_1)
		{ :ok, neuron1_2 } = NeuronProcess.start_link(neuron1_2)
		{ :ok, neuron2_1 } = NeuronProcess.start_link(neuron2_1)

		# this test process will act as the actuator process
		actuator = actuator.pid(self())

		# connect network
		{sensor1, _neuron1_1} = Connector.connect(sensor1, neuron1_1, weight(20))
		{sensor1, _neuron1_2} = Connector.connect(sensor1, neuron1_2, weight(20))
		{sensor2, _neuron1_1} = Connector.connect(sensor2, neuron1_1, weight(20))
		{sensor2, _neuron1_2} = Connector.connect(sensor2, neuron1_2, weight(20))
		{_neuron1_1, _neuron2_1} = Connector.connect(neuron1_1, neuron2_1, weight(20))
		{_neuron1_2, _neuron2_1} = Connector.connect(neuron1_2, neuron2_1, weight(20))
		{_neuron2_1, _actuator} = Connector.connect(neuron2_1, actuator)

		# put values into network, see if expected result comes back
		SensorProcess.send_input(sensor1, 1)
		assert(receive_actuator_output() == :timeout)  # neuron still waiting for other input

		SensorProcess.send_input(sensor2, 2)
		assert(receive_actuator_output() != :timeout)  # should get a value back.  TODO: which value?

	end

	def summation(inputs, weights, bias) do
		TestHelper.summation(inputs, weights, bias)
	end

	def receive_actuator_output() do
		:timeout  # TODO
	end


	def weight(x) do
		x
	end

	def bias(x) do
		x
	end


end