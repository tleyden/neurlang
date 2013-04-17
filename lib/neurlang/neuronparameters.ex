
defrecord NeuronParameters, activation_function: nil, weights: nil, bias: nil do
    @moduledoc """
    The parameters of a neuron

    * `activation_function` - the function that is used in output calculation, typically sigmoid.
                              this is the function itself, not the name of a function

    * `weights` - the weight vector that will be applied to the inputs

    * `bias` - the bias value that will be added to the value after the input * weights is calculated

    """
end