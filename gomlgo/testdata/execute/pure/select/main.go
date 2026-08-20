package main

func receive(ch <-chan int, done chan<- int) {
	value := <-ch
	done <- value
}

func send(ch chan<- int) {
	ch <- 12
}

func main() {
	ready := make(chan int, 1)
	ready <- 4
	select {
	case value := <-ready:
		println("recv", value)
	case ready <- 99:
		println("bad-send")
	default:
		println("bad-default")
	}

	var disabled chan int
	select {
	case <-disabled:
		println("bad-nil")
	default:
		println("default")
	}

	closed := make(chan int)
	close(closed)
	select {
	case value, ok := <-closed:
		println("closed", value, ok)
	}

	out := make(chan int)
	done := make(chan int)
	go receive(out, done)
	select {
	case out <- 9:
		println("send-ready")
	}
	println("sent", <-done)

	in := make(chan int)
	go send(in)
	received := 0
	select {
	case received = <-in:
		println("receive-ready", received)
	}

loop:
	for {
		select {
		default:
			println("labeled")
			break loop
		}
	}
}
