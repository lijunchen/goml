package main

type Point struct {
    x int32
    y int32
}

type Ordering int32

type Maybe__int32 struct {
    _tag int32
    _v0_0 int32
}

func main0() int32 {
    var commute_field449 int32
    var inline447 int32 = 5
    commute_field449 = inline447
    var pt__7 Point
    var inline444 int32 = 7
    var inline445 Point = Point{
        x: commute_field449,
        y: inline444,
    }
    pt__7 = inline445
    var t431 int32 = pt__7.x
    var t434 int32
    var inline438 int32 = pt__7.x
    var inline439 int32 = pt__7.y
    var inline442 int32 = inline438 + inline439
    t434 = inline442
    var t435 int32 = t431 + t434
    return t435
}

func main() {
    main0()
}
