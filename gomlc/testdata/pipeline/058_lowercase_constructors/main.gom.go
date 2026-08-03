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
    var commute_field174 int32
    var inline172 int32 = 5
    commute_field174 = inline172
    var pt__7 Point
    var inline169 int32 = 7
    var inline170 Point = Point{
        x: commute_field174,
        y: inline169,
    }
    pt__7 = inline170
    var t156 int32 = pt__7.x
    var t159 int32
    var inline163 int32 = pt__7.x
    var inline164 int32 = pt__7.y
    var inline167 int32 = inline163 + inline164
    t159 = inline167
    var t160 int32 = t156 + t159
    return t160
}

func main() {
    main0()
}
