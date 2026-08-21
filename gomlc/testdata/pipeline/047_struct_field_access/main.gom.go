package main

type Point struct {
    x int32
    y int32
}

type Ordering int32

func main0() int32 {
    var p__2 Point
    var inline434 int32 = 5
    var inline435 int32 = inline434 + 1
    var inline436 Point = Point{
        x: inline434,
        y: inline435,
    }
    p__2 = inline436
    var t422 int32 = p__2.x
    var t423 int32 = t422 + 1
    var t424 int32 = p__2.y
    var t425 int32 = t424 - 2
    var t427 int32
    var inline432 int32 = t423 + t425
    t427 = inline432
    var t428 int32 = t423 + t427
    return t428
}

func main() {
    main0()
}
