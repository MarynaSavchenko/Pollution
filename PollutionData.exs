


defmodule PollutionData do

	def importLinesFromCSV(file) do
		[head|_] = File.read!(file) |> String.split("\n")
		head
	end

	def parse(line) do
		[date,_,_,_,_] = String.split(line,",")
		date |> String.split("-") |> Enum.reverse |> Enum.map(&Integer.parse/1) |> Enum.map(&elem(&1,0)) |> for x<-&elems do {x} end
	end

end
