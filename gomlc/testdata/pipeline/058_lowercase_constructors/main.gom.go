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
    var retv76 Maybe__int32
    var t77 Maybe__int32 = Some{
        _0: value__0,
    }
    retv76 = t77
    return retv76
}

func build_point(x__1 int32, y__2 int32) Point {
    var retv79 Point
    var t80 Point = Point{
        x: x__1,
        y: y__2,
    }
    retv79 = t80
    return retv79
}

func magnitude(p__3 Point) int32 {
    var retv82 int32
    var mtmp68 Point = p__3
    var x69 int32 = mtmp68.x
    var x70 int32 = mtmp68.y
    var y__5 int32 = x70
    var x__4 int32 = x69
    var t83 int32 = x__4 + y__5
    retv82 = t83
    return retv82
}

func main0() int32 {
    var retv85 int32
    var mtmp71 Maybe__int32 = make_some(5)
    var jp87 int32
    switch mtmp71.(type) {
    case Some:
        var x72 int32 = mtmp71.(Some)._0
        var result__6 int32 = x72
        var pt__7 Point = build_point(result__6, 7)
        var t88 int32 = pt__7.x
        var mtmp73 Maybe__int32 = Some{
            _0: t88,
        }
        var jp90 int32
        switch mtmp73.(type) {
        case Some:
            var x74 int32 = mtmp73.(Some)._0
            var value__8 int32 = x74
            var t91 int32 = magnitude(pt__7)
            var t92 int32 = value__8 + t91
            jp90 = t92
        case None:
            jp90 = 0
        default:
            panic("non-exhaustive match")
        }
        jp87 = jp90
    case None:
        jp87 = 0
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func main() {
    main0()
}
