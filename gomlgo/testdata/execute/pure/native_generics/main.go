package main

import "slices"

func main() {
	ints := slices.Clone([]int{1, 2, 3})
	words := slices.Clone([]string{"go", "ml"})
	again := slices.Clone([]int{4, 5})
	explicit := slices.Clone[[]int]([]int{6})
	println(len(ints), ints[0], ints[2])
	println(len(words), words[0], words[1])
	println(len(again), again[0], again[1])
	println(len(explicit), explicit[0])
}
