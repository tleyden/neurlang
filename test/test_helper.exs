ExUnit.start

defmodule Neurlang.TestHelper do

	def summation(inputs, weights, bias) do
		sum_inputs = Enum.reduce inputs, 0, fn(x, acc) -> 
																						x + acc 
																				end 
		sum_weights = Enum.reduce weights, 0, fn(x, acc) -> 
																							x + acc 
																					end 
		sum_inputs + sum_weights + bias
	end



end