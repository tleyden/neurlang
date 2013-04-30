Code.require_file "../test_helper.exs", __FILE__

defmodule NeuralNetworkTest do

  use ExUnit.Case
	use Neurlang

	import Connector, only: [connect: 1]

	test "neural net with one neuron and identify activation function.  feed data through it and check result" do

		# Create nodes
		neuron = Neuron.start_node(bias: 10, activation_function: function(identity/1))
		sensor = Sensor.start_node(sync_function: fake_sensor_data([[1, 1, 1, 1, 1]]))
		actuator = Actuator.start_node([])

		# Wire up network
		connect(from: sensor, to: neuron, weights: [20, 20, 20, 20, 20])
		connect(from: neuron, to: actuator)

		# tap into actuator for testing purposes
		NodeProcess.add_outbound_connection(actuator, self())

		# feed intput into sensor
		NodeProcess.sync(sensor)

		# verify actuator output
		assert actuator_next_output() == 110

	end

	test "neural net which can solve the XNOR problem.  no learning involved (class.coursera.org/ml/lecture/48)" do

		# Create nodes
		sensor_x1 = Sensor.start_node(sync_function: fake_sensor_data( [[0], [0], [1], [1]]))

		sensor_x2 = Sensor.start_node(sync_function: fake_sensor_data( [[0], [1], [0], [1]]))
		neuron_a2_1 = Neuron.start_node(bias: -30, activation_function: function(sigmoid/1))
		neuron_a2_2 = Neuron.start_node(bias: 10, activation_function: function(sigmoid/1))
		neuron_a3_1 = Neuron.start_node(bias: -10, activation_function: function(sigmoid/1))
		actuator = Actuator.start_node([])

		# Wire up network
		connect(from: sensor_x1, to: neuron_a2_1, weights: [20])
		connect(from: sensor_x1, to: neuron_a2_2, weights: [-20])
		connect(from: sensor_x2, to: neuron_a2_1, weights: [20])
		connect(from: sensor_x2, to: neuron_a2_2, weights: [-20])
		connect(from: neuron_a2_1, to: neuron_a3_1, weights: [20])
		connect(from: neuron_a2_2, to: neuron_a3_1, weights: [20])
		connect(from: neuron_a3_1, to: actuator) 

		# tap into actuator for testing purposes
		NodeProcess.add_outbound_connection(actuator, self())

		# x1 = 0, x2 = 0 -> 1
		sync_sensors(sensor_x1, sensor_x2)
		assert actuator_next_output() > 0.99

		# x1 = 0, x2 = 1 -> 0
		sync_sensors(sensor_x1, sensor_x2)
		assert actuator_next_output() < 0.01

		# x1 = 1, x2 = 0 -> 0
		sync_sensors(sensor_x1, sensor_x2)
		assert actuator_next_output() < 0.01

		# x1 = 1, x2 = 1 -> 1
		sync_sensors(sensor_x1, sensor_x2)
		assert actuator_next_output() > 0.99

	end

	def fake_sensor_data(outputs) do
		MathUtil.create_generator(outputs)
	end

	def actuator_next_output() do
		receive do
			{_pid, :forward, [output]} -> 
				output
			any ->
				assert false, "Got unexpected message: #{inspect(any)}"
		after
			1000 -> assert false, "Did not receive any output from actuator in time"
		end

	end

	def sync_sensors(sensor_x1, sensor_x2) do
		NodeProcess.sync(sensor_x1)
		NodeProcess.sync(sensor_x2)
	end

	def identity(x) do
		x
	end

	def sigmoid(x) do
		MathUtil.sigmoid(x)
	end

end


