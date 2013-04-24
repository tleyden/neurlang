Code.require_file "../test_helper.exs", __FILE__

defmodule NeuralNetworkTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.MathUtil, as: MathUtil
	alias Neurlang.NodeProcess, as: NeuronProcess
	alias Neurlang.Sensor, as: Sensor
	alias Neurlang.NodeProcess, as: SensorProcess
	alias Neurlang.Actuator, as: Actuator
	alias Neurlang.NodeProcess, as: ActuatorProcess

	test "create a full neural net with one neuron and feed data through it" do

		# Create nodes
		neuron = Neuron.new( id: make_ref(), bias: 10, activation_function: function(identity/1) )
		neuron = NeuronProcess.start_link( neuron)

		sensor = Sensor.new( id: make_ref(), sync_function: function(sync_function/0) )
		sensor = SensorProcess.start_link( sensor ) 

		actuator = Actuator.new( id: make_ref() )
		actuator = ActuatorProcess.start_link( actuator )

		# Wire up network
		sensor = SensorProcess.add_outbound_connection( sensor, neuron ) 
		neuron = NeuronProcess.add_inbound_connection( neuron, sensor, weight([20, 20, 20, 20, 20]) ) 
		neuron = NeuronProcess.add_outbound_connection( neuron, actuator )
		actuator = ActuatorProcess.add_inbound_connection( actuator, neuron )

		# tap into actuator for testing purposes
		_actuator = ActuatorProcess.add_outbound_connection( actuator, MockNode.new( pid: self() ) )

		# feed intput into sensor
		SensorProcess.sync(sensor)

		# verify actuator output
		assert actuator_next_output() == 110

	end

	test "neural net which can solve the XNOR problem.  no learning involved (class.coursera.org/ml/lecture/48)" do

		sensor_x1_val_generator = MathUtil.create_generator( [[0], [0], [1], [1]] )
		sensor_x1 = Sensor.new( id: make_ref(), sync_function: sensor_x1_val_generator )
		sensor_x1 = SensorProcess.start_link( sensor_x1 ) 

		sensor_x2_val_generator = MathUtil.create_generator( [[0], [1], [0], [1]] )
		sensor_x2 = Sensor.new( id: make_ref(), sync_function: sensor_x2_val_generator )
		sensor_x2 = SensorProcess.start_link( sensor_x2 ) 

		neuron_a2_1 = Neuron.new( id: make_ref(), bias: -30, activation_function: function(sigmoid/1) )
		neuron_a2_1 = NeuronProcess.start_link( neuron_a2_1 )

		neuron_a2_2 = Neuron.new( id: make_ref(), bias: 10, activation_function: function(sigmoid/1) )
		neuron_a2_2 = NeuronProcess.start_link( neuron_a2_2 )

		neuron_a3_1 = Neuron.new( id: make_ref(), bias: -10, activation_function: function(sigmoid/1) )
		neuron_a3_1 = NeuronProcess.start_link( neuron_a3_1 )

		actuator = Actuator.new( id: make_ref() )
		actuator = ActuatorProcess.start_link( actuator )

		# Wire up network
		sensor_x1 = SensorProcess.add_outbound_connection( sensor_x1, neuron_a2_1 ) 
		neuron_a2_1 = NeuronProcess.add_inbound_connection( neuron_a2_1, sensor_x1, weight([20]) )

		sensor_x1 = SensorProcess.add_outbound_connection( sensor_x1, neuron_a2_2 )
 		neuron_a2_2 = NeuronProcess.add_inbound_connection( neuron_a2_2, sensor_x1, weight([-20]) )

		sensor_x2 = SensorProcess.add_outbound_connection( sensor_x2, neuron_a2_1 ) 
		neuron_a2_1 = NeuronProcess.add_inbound_connection( neuron_a2_1, sensor_x2, weight([20]) )

		sensor_x2 = SensorProcess.add_outbound_connection( sensor_x2, neuron_a2_2 )
 		neuron_a2_2 = NeuronProcess.add_inbound_connection( neuron_a2_2, sensor_x2, weight([-20]) )

		neuron_a2_1 = NeuronProcess.add_outbound_connection( neuron_a2_1, neuron_a3_1 )
		neuron_a3_1 = NeuronProcess.add_inbound_connection( neuron_a3_1, neuron_a2_1, weight([20]) )

		neuron_a2_2 = NeuronProcess.add_outbound_connection( neuron_a2_2, neuron_a3_1 )
		neuron_a3_1 = NeuronProcess.add_inbound_connection( neuron_a3_1, neuron_a2_2, weight([20]) )

		neuron_a3_1 = NeuronProcess.add_outbound_connection( neuron_a3_1, actuator )
		actuator = ActuatorProcess.add_inbound_connection( actuator, neuron_a3_1 )

		# tap into actuator for testing purposes
		_actuator = ActuatorProcess.add_outbound_connection( actuator, MockNode.new( pid: self() ) )

		# x1 = 0, x2 = 0 -> 1
		SensorProcess.sync(sensor_x1)
		SensorProcess.sync(sensor_x2)
		assert actuator_next_output() > 0.99

		# x1 = 0, x2 = 1 -> 0
		SensorProcess.sync(sensor_x1)
		SensorProcess.sync(sensor_x2)
		assert actuator_next_output() < 0.01

		# x1 = 1, x2 = 0 -> 0
		SensorProcess.sync(sensor_x1)
		SensorProcess.sync(sensor_x2)
		assert actuator_next_output() < 0.01

		# x1 = 1, x2 = 1 -> 1
		SensorProcess.sync(sensor_x1)
		SensorProcess.sync(sensor_x2)
		assert actuator_next_output() > 0.99

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

	def identity(x) do
		x
	end

	def sigmoid(x) do
		MathUtil.sigmoid(x)
	end

	def sync_function() do
		[1, 1, 1, 1, 1]
	end 

	def weight(x) do
		x
	end

end

defrecord MockNode, pid: nil do
	@defmodule """
  Useful to allow neurons and actuators send messages to the test process
  """
end
