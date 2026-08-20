package main

import "sync"

func main() {
	var mutex sync.Mutex
	condition := sync.NewCond(&mutex)
	condition.L.Lock()
	condition.L.Unlock()
	println(true)
}
