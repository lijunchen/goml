package main

type Point struct {
    x int32
    y int32
}

func main0() struct{} {
    var ret5 struct{}
    ret5 = struct{}{}
    return ret5
}

func main() {
    main0()
}
