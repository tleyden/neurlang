Code.require_file "../test_helper.exs", __FILE__

defmodule NeuralNetworkTest do

  use ExUnit.Case
	use Neurlang

	import Connector, only: [connect: 1]

	test "create a full neural net with one neuron and feed data through it" do

		# Create nodes
		neuron = Neuron.start_node( id: make_ref(), bias: 10, activation_function: function(identity/1) )
		sensor = Sensor.start_node( id: make_ref(), sync_function: fake_sensor_data( [ [1, 1, 1, 1, 1] ] ) )
		actuator = Actuator.start_node( id: make_ref() )

		# Wire up network
		{ sensor, _neuron } = connect( from: sensor, to: neuron, weights: [20, 20, 20, 20, 20] )
		{ _neuron, actuator } = connect( from: neuron, to: actuator )

		# tap into actuator for testing purposes
		_actuator = ActuatorProcess.add_outbound_connection( actuator, MockNode.new( pid: self() ) )

		# feed intput into sensor
		SensorProcess.sync(sensor)

		# verify actuator output
		assert actuator_next_output() == 110

	end

	test "neural net which can solve the XNOR problem.  no learning involved (class.coursera.org/ml/lecture/48)" do

		# Create nodes
		sensor_x1 = Sensor.start_node( id: make_ref(), sync_function: fake_sensor_data( [[0], [0], [1], [1]] ) )
		sensor_x2 = Sensor.start_node( id: make_ref(), sync_function: fake_sensor_data( [[0], [1], [0], [1]] ) )
		neuron_a2_1 = Neuron.start_node( id: make_ref(), bias: -30, activation_function: function(sigmoid/1) )
		neuron_a2_2 = Neuron.start_node( id: make_ref(), bias: 10, activation_function: function(sigmoid/1) )
		neuron_a3_1 = Neuron.start_node( id: make_ref(), bias: -10, activation_function: function(sigmoid/1) )
		actuator = Actuator.start_node( id: make_ref() )

		# Wire up network
		{ sensor_x1, _neuron_a2_1 } = connect( from: sensor_x1, to: neuron_a2_1, weights: [20] )
		{ sensor_x1, _neuron_a2_2 } = connect( from: sensor_x1, to: neuron_a2_2, weights: [-20] )
		{ sensor_x2, _neuron_a2_1 } = connect( from: sensor_x2, to: neuron_a2_1, weights: [20] )
		{ sensor_x2, _neuron_a2_2 } = connect( from: sensor_x2, to: neuron_a2_2, weights: [-20] )
		{ _neuron_a2_1, _neuron_a3_1 } = connect( from: neuron_a2_1, to: neuron_a3_1, weights: [20] )
		{ _neuron_a2_2, _neuron_a3_1 } = connect( from: neuron_a2_2, to: neuron_a3_1, weights: [20] )
		{ _neuron_a3_1, actuator } = connect( from: neuron_a3_1, to: actuator ) 

		# tap into actuator for testing purposes
		_actuator = ActuatorProcess.add_outbound_connection( actuator, MockNode.new( pid: self() ) )

		# x1 = 0, x2 = 0 -> 1
		sync_sensors( sensor_x1, sensor_x2 )
		assert actuator_next_output() > 0.99

		# x1 = 0, x2 = 1 -> 0
		sync_sensors( sensor_x1, sensor_x2 )
		assert actuator_next_output() < 0.01

		# x1 = 1, x2 = 0 -> 0
		sync_sensors( sensor_x1, sensor_x2 )
		assert actuator_next_output() < 0.01

		# x1 = 1, x2 = 1 -> 1
		sync_sensors( sensor_x1, sensor_x2 )
		assert actuator_next_output() > 0.99

	end

	def fake_sensor_data(outputs) do
		MathUtil.create_generator( outputs )
	end

	def actuator_next_output() do
		receive do
			{ _pid, :forward, [ output ] } -> 
				output
			any ->
				assert false, "Got unexpected message: #{inspect(any)}"
		after
			1000 -> assert false, "Did not receive any output from actuator in time"
		end

	end

	def sync_sensors(sensor_x1, sensor_x2) do
		SensorProcess.sync(sensor_x1)
		SensorProcess.sync(sensor_x2)
	end

	def identity(x) do
		x
	end

	def sigmoid(x) do
		MathUtil.sigmoid(x)
	end

end

defrecord MockNode, pid: nil do
	@defmodule """
  Useful to allow neurons and actuators send messages to the test process
  """
end
