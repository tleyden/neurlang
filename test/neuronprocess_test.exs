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

		neuron = Neuron.new(id: make_ref(), bias: bias(10), activation_function: function(:identity, 1))
		neuron = NeuronProcess.start_link(neuron)

		sensor = Sensor.new(id: make_ref(), output_vector_length: 5)
		sensor = SensorProcess.start_link(sensor) 

		actuator = Actuator.new(id: make_ref())
		actuator = ActuatorProcess.start_link(actuator)
	
		sensor = SensorProcess.add_outbound_connection(sensor, neuron) 
		neuron = NeuronProcess.add_inbound_connection(neuron, sensor, weight([20, 20, 20, 20, 20])) 
		
		neuron = NeuronProcess.add_outbound_connection(neuron, actuator)
		actuator = ActuatorProcess.add_inbound_connection(actuator, neuron)

		SensorProcess.sync_with_outputs(sensor, [1, 1, 1, 1, 1])
		state = ActuatorProcess.get_current_state(actuator)
		
	  # TODO: method which waits until barrier is full, then gets the output
		# received = todo(state)
		# assert(received == 30)
		:timer.sleep(5000)  # otherwise test exists while stuff still running

	end

	def identity(x) do
		x
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