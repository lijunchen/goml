package main

func wait(ch <-chan int, result chan<- string) {
	select {
	case <-ch:
		result <- "blocked-first"
	case <-ch:
		result <- "blocked-second"
	}
}

func send(ch chan<- int) {
	ch <- 3
}

func main() {
	first := make(chan int, 1)
	second := make(chan int, 1)
	first <- 1
	second <- 2
	select {
	case value := <-first:
		println("first", value)
	case value := <-second:
		println("second", value)
	}

	blocked := make(chan int)
	result := make(chan string)
	go wait(blocked, result)
	go send(blocked)
	println(<-result)
}
