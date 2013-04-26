defmodule Neurlang do
  defmacro __using__(_) do
    quote do
			alias Neurlang.Constructor, as: Constructor
			alias Neurlang.ConnectedNode, as: ConnectedNode
			alias Neurlang.Accumulator, as: Accumulator
      alias Neurlang.Neuron, as: Neuron
      alias Neurlang.NodeProcess, as: NeuronProcess
      alias Neurlang.Sensor, as: Sensor
      alias Neurlang.NodeProcess, as: SensorProcess
      alias Neurlang.Actuator, as: Actuator
      alias Neurlang.NodeProcess, as: ActuatorProcess
    end
  end

  @type neuro_node :: Actuator.t | Neuron.t | Sensor.t
  @type barrier_entry :: {pid, list(number)}

end
