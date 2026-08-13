package main

import "strings"

func main() {
	ready := make(chan bool, 1)
	ready <- true
	<-ready
	defer func() {
		println(recover() != nil)
	}()
	strings.Repeat("go", -1)
	println("unreachable")
}
