Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronHelperTest do

  use ExUnit.Case
	alias Neurlang.NeuronHelper, as: NeuronHelper

	test "compute output" do 
		input_vector1 = [10, 10]
		weight_vector1 = [5, 5]
		input_vector2 = [20, 20]
		weight_vector2 = [2, 2]
		weighted_inputs = [{input_vector1, weight_vector1}, {input_vector2, weight_vector2}]
		bias = 50
		activation_function = fn x -> x * 2 end
		result = NeuronHelper.compute_output(weighted_inputs, bias, activation_function) 
		assert result == 460
	end

	test "dot product" do
		dot_product = NeuronHelper.dot_product([2, 2], [4, 4])
		assert dot_product == 16
	end

end