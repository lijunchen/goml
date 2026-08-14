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
    var commute_field220 int32
    var inline218 int32 = 5
    commute_field220 = inline218
    var pt__7 Point
    var inline215 int32 = 7
    var inline216 Point = Point{
        x: commute_field220,
        y: inline215,
    }
    pt__7 = inline216
    var t202 int32 = pt__7.x
    var t205 int32
    var inline209 int32 = pt__7.x
    var inline210 int32 = pt__7.y
    var inline213 int32 = inline209 + inline210
    t205 = inline213
    var t206 int32 = t202 + t205
    return t206
}

func main() {
    main0()
}
