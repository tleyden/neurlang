Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronProcessTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.NeuronProcessState, as: NeuronProcessState
	alias Neurlang.TestHelper, as: TestHelper

	test "redeisgn neuron" do

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
		cortex = Cortex.new(id: make_ref())
		{sensor1, neuron1_1} = Cortex.connect(cortex, sensor1, neuron1_1, weight(20))
		{sensor1, neuron1_2} = Cortex.connect(cortex, sensor1, neuron1_2, weight(20))
		{sensor2, neuron1_1} = Cortex.connect(cortex, sensor2, neuron1_1, weight(20))
		{sensor2, neuron1_2} = Cortex.connect(cortex, sensor2, neuron1_2, weight(20))
		{neuron1_1, neuron2_1} = Cortex.connect(cortex, neuron1_1, neuron2_1, weight(20))
		{neuron1_2, neuron2_1} = Cortex.connect(cortex, neuron1_2, neuron2_1, weight(20))
		{neuron2_1, actuator} = Cortex.connect(cortex, neuron2_1, actuator)

		# put values into network, see if expected result comes back
		SensorProcess.feed_forward(sensor1, 1)
		assert(receive_actuator_output() == :timeout)  # neuron still waiting for other input

		SensorProcess.feed_forward(sensor2, 2)
		assert(receive_actuator_output() != :timeout)  

	end

	test "neuron process produces output when enough inputs provided" do

		# OLD!!

		weights = [2,2]
		bias = 100
    parameters = NeuronParameters.new(activation_function: function(:summation, 3),
																		  weights: weights,
																		  bias: bias)

		neuron_process_state = NeuronProcessState.new(parameters: parameters, 
																									input_nodes: [:fakepid, self()],
																								  output_nodes: [self()])
		{ :ok, neuron_pid } = NeuronProcess.start_link(neuron_process_state)

		NeuronProcess.send_input(neuron_pid, {:fakepid, 1})
		assert(receive_output() == :timeout)  # neuron still waiting for other input

		NeuronProcess.send_input(neuron_pid, {:unlisted_pid, 10})
		assert(receive_output() == :timeout)  # doesn't know about this pid, should ignore

		NeuronProcess.send_input(neuron_pid, {self(), 2})
		assert(receive_output() == 107)

	end

	def summation(inputs, weights, bias) do
		TestHelper.summation(inputs, weights, bias)
	end

	def weight(x) do
		x
	end

	def bias(x) do
		x
	end

	def receive_actuator_output() do
		:timeout
	end

	def receive_output() do

		received = receive do
			{ :output, value} -> 
				value
		after
			1000 ->
				IO.puts "timeout waiting for message"
				:timeout
		end
		received

	end



end