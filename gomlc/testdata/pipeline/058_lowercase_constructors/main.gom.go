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
    var commute_field225 int32
    var inline223 int32 = 5
    commute_field225 = inline223
    var pt__7 Point
    var inline220 int32 = 7
    var inline221 Point = Point{
        x: commute_field225,
        y: inline220,
    }
    pt__7 = inline221
    var t207 int32 = pt__7.x
    var t210 int32
    var inline214 int32 = pt__7.x
    var inline215 int32 = pt__7.y
    var inline218 int32 = inline214 + inline215
    t210 = inline218
    var t211 int32 = t207 + t210
    return t211
}

func main() {
    main0()
}
