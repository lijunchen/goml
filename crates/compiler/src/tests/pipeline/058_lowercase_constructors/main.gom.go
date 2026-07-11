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
    var retv15 option__int32
    var t16 option__int32 = some{
        _0: value__0,
    }
    retv15 = t16
    return retv15
}

func build_point(x__1 int32, y__2 int32) point {
    var retv18 point
    var t19 point = point{
        x: x__1,
        y: y__2,
    }
    retv18 = t19
    return retv18
}

func magnitude(p__3 point) int32 {
    var retv21 int32
    var mtmp7 point = p__3
    var x8 int32 = mtmp7.x
    var x9 int32 = mtmp7.y
    var y__5 int32 = x9
    var x__4 int32 = x8
    var t22 int32 = x__4 + y__5
    retv21 = t22
    return retv21
}

func main0() int32 {
    var retv24 int32
    var mtmp10 option__int32 = make_some(5)
    var jp26 int32
    switch mtmp10.(type) {
    case some:
        var x11 int32 = mtmp10.(some)._0
        var result__6 int32 = x11
        var pt__7 point = build_point(result__6, 7)
        var t27 int32 = pt__7.x
        var mtmp12 option__int32 = some{
            _0: t27,
        }
        var jp29 int32
        switch mtmp12.(type) {
        case some:
            var x13 int32 = mtmp12.(some)._0
            var value__8 int32 = x13
            var t30 int32 = magnitude(pt__7)
            var t31 int32 = value__8 + t30
            jp29 = t31
        case none:
            jp29 = 0
        default:
            panic("non-exhaustive match")
        }
        jp26 = jp29
    case none:
        jp26 = 0
    default:
        panic("non-exhaustive match")
    }
    retv24 = jp26
    return retv24
}

func main() {
    main0()
}
