package main

type Point struct {
    x int32
    y int32
}

type GoError = error

func main0() struct{} {
    return struct{}{}
}

func main() {
    main0()
}
