package main

func caught(name string, function func()) {
	defer func() {
		value := recover()
		println(name, value != nil)
	}()
	defer println(name, "defer")
	function()
}

func main() {
	caught("divide", func() {
		zero := 0
		println(1 / zero)
	})
	caught("index", func() {
		values := []int{1}
		println(values[1])
	})
	caught("pointer", func() {
		var value *int
		println(*value)
	})
	caught("slice", func() {
		length := -1
		println(make([]int, length))
	})
	println("done")
}
