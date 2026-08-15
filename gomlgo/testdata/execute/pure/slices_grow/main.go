package main

import "slices"

type item struct {
	value int
}

func main() {
	values := []item{{value: 7}}
	values = slices.Grow(values, 4)
	println(len(values), cap(values), values[0].value)
}
