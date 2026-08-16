package main

import "context"

func main() {
	ready := make(chan struct{})
	started := make(chan struct{})
	go func() {
		<-started
		close(ready)
	}()
	done := context.Background().Done()
	started <- struct{}{}
	select {
	case <-ready:
		println("ready")
	case <-done:
		println("canceled")
	}
	select {
	case <-context.Background().Done():
		println("unexpected")
	default:
		println("default")
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	select {
	case <-ctx.Done():
		println("done")
	default:
		println("unexpected default")
	}
}
