Code.require_file "../test_helper.exs", __FILE__

defmodule ActuatorTest do
  use ExUnit.Case
	alias Neurlang.Neuron, as: Neuron
	alias Neurlang.Sensor, as: Sensor
	alias Neurlang.Actuator, as: Actuator
	alias Neurlang.ConnectedNode, as: ConnectedNode
	alias Neurlang.Barrier, as: Barrier

  test "barrier satisfied" do
		
		actuator = Actuator.new(pid: :actuatorpid)
		neuron = Neuron.new(pid: :neuronpid)
		IO.puts "adding inbound connection"
		actuator = ConnectedNode.add_inbound_connection( actuator, neuron )
		IO.puts "added inbound connection"
		barrier = HashDict.new()
		barrier = Dict.put( barrier, :neuronpid, :fake_input)
		actuator = actuator.barrier( barrier )
		assert Barrier.is_barrier_satisfied?(actuator) == true
		IO.puts "done"
	end


	
end
