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
    var t7 option__int32 = some{
        _0: value__0,
    }
    return t7
}

func build_point(x__1 int32, y__2 int32) point {
    var t8 point = point{
        x: x__1,
        y: y__2,
    }
    return t8
}

func magnitude(p__3 point) int32 {
    var mtmp0 point = p__3
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__5 int32 = x2
    var x__4 int32 = x1
    var t9 int32 = x__4 + y__5
    return t9
}

func main0() int32 {
    var mtmp3 option__int32 = make_some(5)
    var jp11 int32
    switch mtmp3.(type) {
    case some:
        var x4 int32 = mtmp3.(some)._0
        var result__6 int32 = x4
        var pt__7 point = build_point(result__6, 7)
        var t12 int32 = pt__7.x
        var mtmp5 option__int32 = some{
            _0: t12,
        }
        var jp14 int32
        switch mtmp5.(type) {
        case some:
            var x6 int32 = mtmp5.(some)._0
            var value__8 int32 = x6
            var t15 int32 = magnitude(pt__7)
            var t16 int32 = value__8 + t15
            jp14 = t16
        case none:
            jp14 = 0
        default:
            panic("non-exhaustive match")
        }
        jp11 = jp14
        return jp11
    case none:
        jp11 = 0
        return jp11
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
