defmodule Neurlang.MathUtil do

	## API

	@doc """
  Compute the dot product of the given vectors
  """
	@spec dot_product([number], [number]) :: number
	def dot_product(inputs, weights) do
		dot_product(inputs, weights, 0)
	end

	## Private

	@spec dot_product([number], [number], number) :: number
	defp dot_product([i|inputs], [w|weights], acc) do
		dot_product(inputs, weights, i*w + acc)
	end

	defp dot_product([], [], acc) do
		acc
	end



end