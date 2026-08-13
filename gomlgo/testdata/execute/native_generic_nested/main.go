package main

import "slices"

func clone[T any](values []T) []T {
	return slices.Clone(values)
}

func main() {
	ints := clone([]int{7, 8})
	words := clone([]string{"go", "ml"})
	println(ints[0], ints[1], words[0], words[1])
}
