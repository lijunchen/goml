package main

type point struct {
    x int32
    y int32
}

type option__int32 interface {
    isoption__int32()
}

type some struct {
    _0 int32
}

func (_ some) isoption__int32() {}

type none struct {}

func (_ none) isoption__int32() {}

func make_some(value__0 int32) option__int32 {
    var retv66 option__int32
    var t67 option__int32 = some{
        _0: value__0,
    }
    retv66 = t67
    return retv66
}

func build_point(x__1 int32, y__2 int32) point {
    var retv69 point
    var t70 point = point{
        x: x__1,
        y: y__2,
    }
    retv69 = t70
    return retv69
}

func magnitude(p__3 point) int32 {
    var retv72 int32
    var mtmp58 point = p__3
    var x59 int32 = mtmp58.x
    var x60 int32 = mtmp58.y
    var y__5 int32 = x60
    var x__4 int32 = x59
    var t73 int32 = x__4 + y__5
    retv72 = t73
    return retv72
}

func main0() int32 {
    var retv75 int32
    var mtmp61 option__int32 = make_some(5)
    var jp77 int32
    switch mtmp61.(type) {
    case some:
        var x62 int32 = mtmp61.(some)._0
        var result__6 int32 = x62
        var pt__7 point = build_point(result__6, 7)
        var t78 int32 = pt__7.x
        var mtmp63 option__int32 = some{
            _0: t78,
        }
        var jp80 int32
        switch mtmp63.(type) {
        case some:
            var x64 int32 = mtmp63.(some)._0
            var value__8 int32 = x64
            var t81 int32 = magnitude(pt__7)
            var t82 int32 = value__8 + t81
            jp80 = t82
        case none:
            jp80 = 0
        default:
            panic("non-exhaustive match")
        }
        jp77 = jp80
    case none:
        jp77 = 0
    default:
        panic("non-exhaustive match")
    }
    retv75 = jp77
    return retv75
}

func main() {
    main0()
}
