Code.require_file "../test_helper.exs", __FILE__

defmodule MathUtilTest do

  use ExUnit.Case
	alias Neurlang.MathUtil, as: MathUtil

	test "dot product" do
		dot_product = MathUtil.dot_product([2, 2], [4, 4])
		assert dot_product == 16
	end

end