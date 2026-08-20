package main

import "time"

func main() {
	select {
	case <-time.After(2 * time.Millisecond):
		println("timer")
	case <-time.After(time.Second):
		println("late")
	}
}
