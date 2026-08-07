package main

type Point struct {
    x int32
    y int32
}

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
    var commute_field210 int32
    var inline208 int32 = 5
    commute_field210 = inline208
    var pt__7 Point
    var inline205 int32 = 7
    var inline206 Point = Point{
        x: commute_field210,
        y: inline205,
    }
    pt__7 = inline206
    var t192 int32 = pt__7.x
    var t195 int32
    var inline199 int32 = pt__7.x
    var inline200 int32 = pt__7.y
    var inline203 int32 = inline199 + inline200
    t195 = inline203
    var t196 int32 = t192 + t195
    return t196
}

func main() {
    main0()
}
