defmodule Neurlang.Constructor do
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.NodeProcess, as: NeuronProcess
	alias Neurlang.Sensor, as: Sensor
	alias Neurlang.NodeProcess, as: SensorProcess
	alias Neurlang.Actuator, as: Actuator
	alias Neurlang.NodeProcess, as: ActuatorProcess

	def neuron( keywords ) do
		neuron = Neuron.new( keywords )
		neuron = NeuronProcess.start_link( neuron)
		neuron
	end

	def sensor( keywords ) do
		sensor = Sensor.new( keywords )
		sensor = SensorProcess.start_link( sensor)
		sensor
	end

	def actuator( keywords ) do
		actuator = Actuator.new( keywords )
		actuator = ActuatorProcess.start_link( actuator)
		actuator
	end

end