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
    var commute_field193 int32
    var inline191 int32 = 5
    commute_field193 = inline191
    var pt__7 Point
    var inline188 int32 = 7
    var inline189 Point = Point{
        x: commute_field193,
        y: inline188,
    }
    pt__7 = inline189
    var t175 int32 = pt__7.x
    var t178 int32
    var inline182 int32 = pt__7.x
    var inline183 int32 = pt__7.y
    var inline186 int32 = inline182 + inline183
    t178 = inline186
    var t179 int32 = t175 + t178
    return t179
}

func main() {
    main0()
}
