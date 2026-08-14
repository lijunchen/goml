package main

type Point struct {
    x int32
    y int32
}

type Ordering int32

type Maybe__int32 interface {
    isMaybe__int32()
}

type Some struct {
    _0 int32
}

func (_ Some) isMaybe__int32() {}

type None struct {}

func (_ None) isMaybe__int32() {}

func main0() int32 {
    var commute_field446 int32
    var inline444 int32 = 5
    commute_field446 = inline444
    var pt__7 Point
    var inline441 int32 = 7
    var inline442 Point = Point{
        x: commute_field446,
        y: inline441,
    }
    pt__7 = inline442
    var t428 int32 = pt__7.x
    var t431 int32
    var inline435 int32 = pt__7.x
    var inline436 int32 = pt__7.y
    var inline439 int32 = inline435 + inline436
    t431 = inline439
    var t432 int32 = t428 + t431
    return t432
}

func main() {
    main0()
}
