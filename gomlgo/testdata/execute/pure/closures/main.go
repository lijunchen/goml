package main

func twice(value int) int {
	return value * 2
}

func apply(function func(int) int, value int) int {
	return function(value)
}

func factory(initial int) func(int) int {
	total := initial
	return func(delta int) int {
		total += delta
		return total
	}
}

func main() {
	first := factory(1)
	second := factory(10)
	println(first(2), first(3), second(1))

	var factorial func(int) int
	factorial = func(value int) int {
		if value < 2 {
			return 1
		}
		return value * factorial(value-1)
	}
	println(factorial(5), apply(twice, 3), func(value int) int { return value + 4 }(4))

	outer := 7
	middle := func() func() int {
		return func() int {
			return outer
		}
	}
	outer = 9
	println(middle()())

	functions := []func() int{}
	for _, value := range []int{1, 2, 3} {
		functions = append(functions, func() int { return value })
	}
	println(functions[0](), functions[1](), functions[2]())

	shared := 0
	sharedFunctions := []func() int{}
	for _, shared = range []int{4, 5, 6} {
		sharedFunctions = append(sharedFunctions, func() int { return shared })
	}
	println(sharedFunctions[0](), sharedFunctions[1](), sharedFunctions[2]())
	classic := []func() int{}
	for index := 0; index < 3; index++ {
		classic = append(classic, func() int { return index })
	}
	println(classic[0](), classic[1](), classic[2]())
	body := []func() int{}
	for index := 0; index < 3; index++ {
		value := index
		body = append(body, func() int { return value })
	}
	println(body[0](), body[1](), body[2]())
}
