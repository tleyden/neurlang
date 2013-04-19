Code.require_file "../test_helper.exs", __FILE__

defmodule NeuronHelperTest do

  use ExUnit.Case
	alias Neurlang.NeuronHelper, as: NeuronHelper

	test "dot product" do
		dot_product = NeuronHelper.dot_product([2, 2], [4, 4])
		assert dot_product == 16
	end

end