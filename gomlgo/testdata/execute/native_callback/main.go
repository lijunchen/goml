package main

import "strings"

func callbackPanic() {
	defer func() {
		println("panic", recover() != nil)
	}()
	strings.Map(func(value rune) rune {
		panic("callback")
	}, "x")
}

func main() {
	delta := rune(1)
	calls := 0
	mapped := strings.Map(func(value rune) rune {
		calls++
		return value + delta
	}, "abc")
	println(mapped, calls)
	parts := strings.FieldsFunc("a,b;c", func(value rune) bool {
		return value == ',' || value == ';'
	})
	println(len(parts), parts[0], parts[1], parts[2])
	callbackPanic()
}
