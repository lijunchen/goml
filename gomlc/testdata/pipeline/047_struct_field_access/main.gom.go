package main

type Point struct {
    x int32
    y int32
}

type Ordering int32

func main0() int32 {
    var p__2 Point
    var inline431 int32 = 5
    var inline432 int32 = inline431 + 1
    var inline433 Point = Point{
        x: inline431,
        y: inline432,
    }
    p__2 = inline433
    var t419 int32 = p__2.x
    var t420 int32 = t419 + 1
    var t421 int32 = p__2.y
    var t422 int32 = t421 - 2
    var t424 int32
    var inline429 int32 = t420 + t422
    t424 = inline429
    var t425 int32 = t420 + t424
    return t425
}

func main() {
    main0()
}
