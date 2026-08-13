package main

import "os"

func main() {
	ready := make(chan bool, 1)
	ready <- true
	<-ready
	defer println("unreachable")
	os.Exit(7)
}
