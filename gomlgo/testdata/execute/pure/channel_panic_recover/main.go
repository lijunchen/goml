package main

func sendClosed() {
	defer func() {
		println("send", recover() != nil)
	}()
	channel := make(chan int)
	close(channel)
	channel <- 1
}

func closeNil() {
	defer func() {
		println("nil", recover() != nil)
	}()
	var channel chan int
	close(channel)
}

func closeClosed() {
	defer func() {
		println("closed", recover() != nil)
	}()
	channel := make(chan int)
	close(channel)
	close(channel)
}

func main() {
	sendClosed()
	closeNil()
	closeClosed()
}
