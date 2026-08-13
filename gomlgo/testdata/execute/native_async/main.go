package main

import "time"

func sleeper(started chan<- bool) {
	started <- true
	time.Sleep(30 * time.Second)
	println("late")
}

func main() {
	started := make(chan bool)
	go sleeper(started)
	<-started
	println("main")
}
