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

		sensor = Sensor.new( id: make_ref(), output_vector_length: 5, sync_function: function(sync_function/0) )
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

		# wait for output from actuator 
		receive do
			{_pid, :forward, output} -> 
				assert output == [ 110 ]  
			any ->
				assert false, "Got unexpected message: #{inspect(any)}"
		after
			1000 -> assert false, "Did not receive any output from actuator in time"
		end

	end


	test "neural net which can solve the XNOR problem.  no learning involved (class.coursera.org/ml/lecture/48)" do

		#sensor = Sensor.new( id: make_ref(), output_vector_length: 1, sync_function: function(xnor_sync_function/1) )
		#sensor = SensorProcess.start_link( sensor ) 

		next = MathUtil.create_generator([0, 1, 1, 0])
		assert next.() == 0
		assert next.() == 1
		assert next.() == 1
		assert next.() == 0

	end

	def identity(x) do
		x
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
