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
    var retv30 option__int32
    var t31 option__int32 = some{
        _0: value__0,
    }
    retv30 = t31
    return retv30
}

func build_point(x__1 int32, y__2 int32) point {
    var retv33 point
    var t34 point = point{
        x: x__1,
        y: y__2,
    }
    retv33 = t34
    return retv33
}

func magnitude(p__3 point) int32 {
    var retv36 int32
    var mtmp22 point = p__3
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var y__5 int32 = x24
    var x__4 int32 = x23
    var t37 int32 = x__4 + y__5
    retv36 = t37
    return retv36
}

func main0() int32 {
    var retv39 int32
    var mtmp25 option__int32 = make_some(5)
    var jp41 int32
    switch mtmp25.(type) {
    case some:
        var x26 int32 = mtmp25.(some)._0
        var result__6 int32 = x26
        var pt__7 point = build_point(result__6, 7)
        var t42 int32 = pt__7.x
        var mtmp27 option__int32 = some{
            _0: t42,
        }
        var jp44 int32
        switch mtmp27.(type) {
        case some:
            var x28 int32 = mtmp27.(some)._0
            var value__8 int32 = x28
            var t45 int32 = magnitude(pt__7)
            var t46 int32 = value__8 + t45
            jp44 = t46
        case none:
            jp44 = 0
        default:
            panic("non-exhaustive match")
        }
        jp41 = jp44
    case none:
        jp41 = 0
    default:
        panic("non-exhaustive match")
    }
    retv39 = jp41
    return retv39
}

func main() {
    main0()
}
