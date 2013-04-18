Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronProcessTest do

  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NeuronProcess, as: NeuronProcess
	alias Neurlang.NeuronProcessState, as: NeuronProcessState
	alias Neurlang.TestHelper, as: TestHelper

	test "new v2 neuron" do

		neuron1 = Neuron.new(id: make_ref(), bias: bias(10), activation_function: function(:summation, 3))
		{ :ok, neuron1 } = NeuronProcess.start_link(neuron1)

		mocksensor = MockNode.new(id: make_ref())
		mockactuator = MockNode.new(id: make_ref())
		{ :ok, mocksensor } = MockNodeProcess.start_link(mocksensor)
		{ :ok, mockactuator } = MockNodeProcess.start_link(mockactuator)
		
		{mocksensor, _neuron1} = Connector.connect(mocksensor, neuron1, weight([20]))  
		{_neuron1, mockactuator} = Connector.connect(neuron1, mockactuator)

		MockNodeProcess.send_input(mocksensor, 1)
		received = MockNodeProcess.get_last_received(mockactuator)
		assert(received == 30)

	end

	test "neuron process produces output when enough inputs provided" do

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

	def weight(x) do
		x
	end

	def bias(x) do
		x
	end

end