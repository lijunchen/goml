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
    var commute_field215 int32
    var inline213 int32 = 5
    commute_field215 = inline213
    var pt__7 Point
    var inline210 int32 = 7
    var inline211 Point = Point{
        x: commute_field215,
        y: inline210,
    }
    pt__7 = inline211
    var t197 int32 = pt__7.x
    var t200 int32
    var inline204 int32 = pt__7.x
    var inline205 int32 = pt__7.y
    var inline208 int32 = inline204 + inline205
    t200 = inline208
    var t201 int32 = t197 + t200
    return t201
}

func main() {
    main0()
}
