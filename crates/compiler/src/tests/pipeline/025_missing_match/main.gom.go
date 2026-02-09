package main

type Point struct {
    x int32
    y int32
}

func main0() struct{} {
    var ret4 struct{}
    ret4 = struct{}{}
    return ret4
}

func main() {
    main0()
}
