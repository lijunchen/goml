package main

func lifo() (value int) {
	defer func() {
		value = value*10 + 1
	}()
	defer func() {
		value = value*10 + 2
	}()
	return
}

func named() (value int) {
	defer func() {
		value += 10
	}()
	value = 2
	return
}

func target(value *int) func(int) {
	*value = *value*10 + 1
	return func(argument int) {
		*value = *value*10 + argument
	}
}

func argument(value *int) int {
	*value = *value*10 + 2
	return 2
}

func registration() (value int) {
	defer target(&value)(argument(&value))
	value = value*10 + 3
	return
}

func sum(result *int, values ...int) {
	for _, value := range values {
		*result += value
	}
}

func variadic() (value int) {
	values := []int{1, 2}
	defer sum(&value, values...)
	values[0] = 4
	return
}

func direct() (value int) {
	defer func() {
		value = recover().(int)
	}()
	panic(9)
}

func helper() any {
	return recover()
}

func indirect() (recovered bool) {
	defer func() {
		_ = recover()
	}()
	defer func() {
		recovered = helper() != nil
	}()
	panic("indirect")
}

func nilPanic() (nonNil bool) {
	defer func() {
		nonNil = recover() != nil
	}()
	panic(nil)
}

func typedNil() (nonNil bool) {
	var pointer *int
	defer func() {
		nonNil = recover() != nil
	}()
	panic(pointer)
}

func normal() (nilValue bool) {
	defer func() {
		nilValue = recover() == nil
	}()
	return
}

func once() (first bool, second bool) {
	defer func() {
		first = recover() != nil
		second = recover() == nil
	}()
	panic(5)
}

func deferredRecoverCall() (recovered bool) {
	defer func() {
		recovered = recover() != nil
	}()
	defer recover()
	panic(7)
}

func main() {
	println(lifo(), named(), registration(), variadic())
	println(direct(), indirect())
	println(nilPanic(), typedNil())
	println(recover() == nil, normal())
	first, second := once()
	println(first, second, deferredRecoverCall())
}
