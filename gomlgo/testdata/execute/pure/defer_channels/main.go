package main

func finish(ch chan int, result chan int) int {
	go func() {
		ch <- 7
	}()
	value := <-ch
	select {
	case result <- value + 1:
	}
	return 99
}

func work(ch chan int, result chan int) {
	defer finish(ch, result)
}

func recoverWork(result chan bool) {
	defer func() {
		result <- recover() != nil
	}()
	panic("scheduled")
}

func main() {
	ch := make(chan int)
	result := make(chan int, 1)
	work(ch, result)
	println("defer", <-result)
	recovered := make(chan bool, 1)
	recoverWork(recovered)
	println("recover", <-recovered)
}
