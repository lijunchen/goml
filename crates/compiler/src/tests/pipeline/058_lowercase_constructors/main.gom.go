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
    var retv72 Maybe__int32
    var t73 Maybe__int32 = Some{
        _0: value__0,
    }
    retv72 = t73
    return retv72
}

func build_point(x__1 int32, y__2 int32) Point {
    var retv75 Point
    var t76 Point = Point{
        x: x__1,
        y: y__2,
    }
    retv75 = t76
    return retv75
}

func magnitude(p__3 Point) int32 {
    var retv78 int32
    var mtmp64 Point = p__3
    var x65 int32 = mtmp64.x
    var x66 int32 = mtmp64.y
    var y__5 int32 = x66
    var x__4 int32 = x65
    var t79 int32 = x__4 + y__5
    retv78 = t79
    return retv78
}

func main0() int32 {
    var retv81 int32
    var mtmp67 Maybe__int32 = make_some(5)
    var jp83 int32
    switch mtmp67.(type) {
    case Some:
        var x68 int32 = mtmp67.(Some)._0
        var result__6 int32 = x68
        var pt__7 Point = build_point(result__6, 7)
        var t84 int32 = pt__7.x
        var mtmp69 Maybe__int32 = Some{
            _0: t84,
        }
        var jp86 int32
        switch mtmp69.(type) {
        case Some:
            var x70 int32 = mtmp69.(Some)._0
            var value__8 int32 = x70
            var t87 int32 = magnitude(pt__7)
            var t88 int32 = value__8 + t87
            jp86 = t88
        case None:
            jp86 = 0
        default:
            panic("non-exhaustive match")
        }
        jp83 = jp86
    case None:
        jp83 = 0
    default:
        panic("non-exhaustive match")
    }
    retv81 = jp83
    return retv81
}

func main() {
    main0()
}
