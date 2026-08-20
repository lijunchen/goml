package main

var trace int

func mark(value int) int {
	trace = trace*10 + value
	return trace
}

func pair() (int, int) {
	return mark(3), mark(4)
}

var second = mark(2)
var first = mark(1)
var left, right = pair()

func init() {
	trace = trace*10 + 5
}

func init() {
	trace = trace*10 + 6
}

func main() {
	println(second, first, left, right, trace)
}
