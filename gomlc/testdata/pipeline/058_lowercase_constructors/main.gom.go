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

func make_some(value__0 int32) Maybe__int32 {
    var t164 Maybe__int32 = Some{
        _0: value__0,
    }
    return t164
}

func build_point(x__1 int32, y__2 int32) Point {
    var t167 Point = Point{
        x: x__1,
        y: y__2,
    }
    return t167
}

func magnitude(p__3 Point) int32 {
    var x156 int32 = p__3.x
    var x157 int32 = p__3.y
    var t170 int32 = x156 + x157
    return t170
}

func main0() int32 {
    var mtmp158 Maybe__int32 = make_some(5)
    switch mtmp158.(type) {
    case Some:
        var x159 int32 = mtmp158.(Some)._0
        var pt__7 Point = build_point(x159, 7)
        var t175 int32 = pt__7.x
        var t178 int32 = magnitude(pt__7)
        var t179 int32 = t175 + t178
        return t179
    case None:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
