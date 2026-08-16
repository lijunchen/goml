package main

import "runtime"

func main() {
	buffer := make([]byte, 64)
	length := runtime.Stack(buffer, false)
	println(length > 10 && string(buffer[:10]) == "goroutine ")
}
