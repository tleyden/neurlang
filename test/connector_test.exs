Code.require_file "../test_helper.exs", __FILE__

defmodule ConnectorTest do

  use ExUnit.Case
	alias Neurlang.TestHelper, as: TestHelper
	alias Neurlang.Mock.MockNode, as: MockNode
	alias Neurlang.Mock.MockNodeProcess, as: MockNodeProcess
	alias Neurlang.Connector, as: Connector

	test "connect sensor -> neuron" do

		mocksensor_id = make_ref()
		mockneuron_id = make_ref()
		mocksensor = MockNode.new(id: mocksensor_id)
		mockneuron = MockNode.new(id: mockneuron_id)

		{ :ok, mocksensor_pid } = MockNodeProcess.start_link(mocksensor)
		{ :ok, mockneuron_pid } = MockNodeProcess.start_link(mockneuron)

		mocksensor = mocksensor.pid(mocksensor_pid)
		mockneuron = mockneuron.pid(mockneuron_pid)
		
		{mocksensor, mockneuron} = Connector.connect_weighted(mocksensor, mockneuron, weight([20]))  
		
		assert(length(mockneuron.inbound_connections()) == 1, 
						"neuron should have 1 item in inbound_connections, which is the mocksensor")  
		
		{inbound_pid, _weight} = Enum.at!(mockneuron.inbound_connections(), 0)
		assert( inbound_pid == mocksensor.pid(), "expected to see mocksensor in inbound connections" )

		assert( length( mockneuron.outbound_connections() ) == 0, 
						"the neuron should have no items in outbound_connections" ) 

		assert( length( mocksensor.inbound_connections() ) == 0, 
						"the sensor should have no items in inbound_connections" )
		assert( length( mocksensor.outbound_connections() ) == 1, 
						"the sensor should have 1 item in outbound_connections, the mockneuron" )  

		outbound_pid = Enum.at!(mocksensor.outbound_connections(), 0)
		assert( outbound_pid == mockneuron.pid(), "expected to see mockneuron in outbound connections" )
	
	end

	def weight(x) do
		x
	end

	def bias(x) do
		x
	end

end