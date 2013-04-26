defmodule Neurlang.Constructor do
	use Neurlang

  @spec neuron(Neuron.options) :: Neuron.t
	def neuron(keywords) do
		neuron = Neuron.new(keywords)
		NeuronProcess.start_link(neuron)
	end

  @spec sensor(Sensor.options) :: Sensor.t
	def sensor(keywords) do
		sensor = Sensor.new(keywords)
		SensorProcess.start_link(sensor)
	end

  @spec actuator(Actuator.options) :: Actuator.t
	def actuator(keywords) do
		actuator = Actuator.new(keywords)
		ActuatorProcess.start_link(actuator)
	end

end