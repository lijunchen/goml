package main

func main() {
	blocked := make(chan int)
	<-blocked
}
