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
    var retv12 option__int32
    var t13 option__int32 = some{
        _0: value__0,
    }
    retv12 = t13
    return retv12
}

func build_point(x__1 int32, y__2 int32) point {
    var retv15 point
    var t16 point = point{
        x: x__1,
        y: y__2,
    }
    retv15 = t16
    return retv15
}

func magnitude(p__3 point) int32 {
    var retv18 int32
    var mtmp4 point = p__3
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__5 int32 = x6
    var x__4 int32 = x5
    var t19 int32 = x__4 + y__5
    retv18 = t19
    return retv18
}

func main0() int32 {
    var retv21 int32
    var mtmp7 option__int32 = make_some(5)
    var jp23 int32
    switch mtmp7.(type) {
    case some:
        var x8 int32 = mtmp7.(some)._0
        var result__6 int32 = x8
        var pt__7 point = build_point(result__6, 7)
        var t24 int32 = pt__7.x
        var mtmp9 option__int32 = some{
            _0: t24,
        }
        var jp26 int32
        switch mtmp9.(type) {
        case some:
            var x10 int32 = mtmp9.(some)._0
            var value__8 int32 = x10
            var t27 int32 = magnitude(pt__7)
            var t28 int32 = value__8 + t27
            jp26 = t28
        case none:
            jp26 = 0
        default:
            panic("non-exhaustive match")
        }
        jp23 = jp26
    case none:
        jp23 = 0
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func main() {
    main0()
}
