defmodule Neurlang do
  defmacro __using__(_) do
    quote do
			alias Neurlang.Connector, as: Connector
			alias Neurlang.ConnectedNode, as: ConnectedNode
			alias Neurlang.Accumulator, as: Accumulator
      alias Neurlang.Neuron, as: Neuron
      alias Neurlang.NodeProcess, as: NeuronProcess
      alias Neurlang.Sensor, as: Sensor
      alias Neurlang.NodeProcess, as: SensorProcess
      alias Neurlang.Actuator, as: Actuator
      alias Neurlang.NodeProcess, as: ActuatorProcess
      alias Neurlang.MathUtil, as: MathUtil
    end
  end

  @type neuro_node :: Actuator.t | Neuron.t | Sensor.t
  @type barrier_entry :: {pid, list(number)}

end
