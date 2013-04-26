defmodule Neurlang do
  defmacro __using__(_) do
    quote do
			alias Neurlang.Connector, as: Connector
			alias Neurlang.ConnectedNode, as: ConnectedNode
			alias Neurlang.Accumulator, as: Accumulator
      alias Neurlang.Neuron, as: Neuron
      alias Neurlang.Sensor, as: Sensor
      alias Neurlang.Actuator, as: Actuator
      alias Neurlang.NodeProcess, as: NodeProcess
      alias Neurlang.MathUtil, as: MathUtil
    end
  end

	@type neurlang_node :: Actuator.t | Neuron.t | Sensor.t
	@type barrier_entry :: {pid, list(number)}
	@type handle_call_msg :: { :add_inbound_connection | :add_outbound_connection, {neurlang_node, list(number) } | neurlang_node }

end
