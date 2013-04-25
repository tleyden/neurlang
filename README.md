# Neurlang


A library for constructing Neural Networks in [Elixir](http://elixir-lang.org/) or Erlang.  

Neurlang is essentially a port of the [DXNN2](https://github.com/CorticalComputer/DXNN2) core neural network elements.  

However, unlike [DXNN2](https://github.com/CorticalComputer/DXNN2), Neurlang:

* Is written in Elixir
* Is easily usable in non-TWEANN's (Topology WEight Adapting Neural Networks).  

The goal is that any neural network should be able to be built using the components available in Neurlang.  If you want to build a neural network, and Neurlang is missing something you need, please file an issue with a feature request.

___Status: Neurlang is still in the process of being built, and is therefore is about as stable as the price of bitcoins___

# Using Neurlang

    # Create nodes
    neuron = neuron( id: make_ref(), bias: 10, activation_function: function(identity/1) )
    sensor = sensor( id: make_ref(), sync_function: function(sync_function/0) )
    actuator = actuator( id: make_ref() )

    # Wire up network
    { sensor, _neuron } = connect( from: sensor, to: neuron, weights: [20, 20, 20, 20, 20] )
    { _neuron, actuator } = connect( from: neuron, to: actuator )

    # tap into actuator for testing purposes
    _actuator = ActuatorProcess.add_outbound_connection( actuator, MockNode.new( pid: self() ) )

    # trigger sensor to send fake values
    SensorProcess.sync(sensor)

    # verify actuator output
    assert actuator_next_output() == 110


# Architecture

Neurlang provides three types of neural elements, all of which are <code>gen_server</code> processes which can link up with eachother and exchange messages.

* Sensor 

* Neuron

* Actuator

# Related Work

[DXNN2](https://github.com/CorticalComputer/DXNN2) - Pure Erlang TWEANN (Topology WEight Adapting Neural Network).  Neurlang is essentially a port of the DXNN2 core neural network elements.  

# Related Publications

[Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.

# How to run dialyzer

One time:

    dialyzer -DDIALYZER --output_plt .depsolver.plt --build_plt --apps erts kernel stdlib crypto public_key $ELIXIR/lib/elixir/ebin $ELIXIR/lib/ex_unit/ebin

where $ELIXIR points to your elixir install dir, eg, /usr/local/lib/elixir

Each time:

    dialyzer --quiet --no_check_plt --plt .depsolver.plt -Wunmatched_returns -Werror_handling -Wrace_conditions ebin

or

    dialyzer --quiet --no_check_plt --plt .depsolver.plt -Wunderspecs -Wunmatched_returns -Werror_handling -Wrace_conditions ebin
