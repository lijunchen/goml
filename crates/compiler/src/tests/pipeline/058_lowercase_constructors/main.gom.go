package main

type point struct {
    x int32
    y int32
}

type option__int32 interface {
    isoption__int32()
}

type Some struct {
    _0 int32
}

func (_ Some) isoption__int32() {}

type None struct {}

func (_ None) isoption__int32() {}

func make_some(value__0 int32) option__int32 {
    var retv69 option__int32
    var t70 option__int32 = Some{
        _0: value__0,
    }
    retv69 = t70
    return retv69
}

func build_point(x__1 int32, y__2 int32) point {
    var retv72 point
    var t73 point = point{
        x: x__1,
        y: y__2,
    }
    retv72 = t73
    return retv72
}

func magnitude(p__3 point) int32 {
    var retv75 int32
    var mtmp61 point = p__3
    var x62 int32 = mtmp61.x
    var x63 int32 = mtmp61.y
    var y__5 int32 = x63
    var x__4 int32 = x62
    var t76 int32 = x__4 + y__5
    retv75 = t76
    return retv75
}

func main0() int32 {
    var retv78 int32
    var mtmp64 option__int32 = make_some(5)
    var jp80 int32
    switch mtmp64.(type) {
    case Some:
        var x65 int32 = mtmp64.(Some)._0
        var result__6 int32 = x65
        var pt__7 point = build_point(result__6, 7)
        var t81 int32 = pt__7.x
        var mtmp66 option__int32 = Some{
            _0: t81,
        }
        var jp83 int32
        switch mtmp66.(type) {
        case Some:
            var x67 int32 = mtmp66.(Some)._0
            var value__8 int32 = x67
            var t84 int32 = magnitude(pt__7)
            var t85 int32 = value__8 + t84
            jp83 = t85
        case None:
            jp83 = 0
        default:
            panic("non-exhaustive match")
        }
        jp80 = jp83
    case None:
        jp80 = 0
    default:
        panic("non-exhaustive match")
    }
    retv78 = jp80
    return retv78
}

func main() {
    main0()
}
