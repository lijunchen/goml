package basic

import "fmt"

type Pair[A, B any] struct {
	First  A
	Second B
}

func Print[A, B any](pair Pair[A, B]) {
	fmt.Println(pair.First, pair.Second)
}
