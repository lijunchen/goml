package main

var initialized = make(chan int)

func init() {
	go func() {
		initialized <- 6
	}()
}

func send(ch chan<- int) {
	ch <- 7
	ch <- 8
	close(ch)
}

func main() {
	println(<-initialized)
	buffered := make(chan int, 2)
	buffered <- 1
	buffered <- 2
	println(len(buffered), cap(buffered))
	println(<-buffered, <-buffered, len(buffered), cap(buffered))

	unbuffered := make(chan int)
	go send(unbuffered)
	first := <-unbuffered
	second, secondOK := <-unbuffered
	zero, zeroOK := <-unbuffered
	println(first, second, secondOK, zero, zeroOK)

	ranged := make(chan int, 2)
	ranged <- 4
	ranged <- 5
	close(ranged)
	total := 0
	for value := range ranged {
		total += value
	}
	println("range", total)

	done := make(chan bool)
	value := 3
	go func() {
		value = value + 4
		done <- true
	}()
	<-done
	println(value)
}
