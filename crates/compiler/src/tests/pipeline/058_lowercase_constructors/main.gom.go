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

type GoError = error

func make_some(value__0 int32) option__int32 {
    var retv8 option__int32
    var t9 option__int32 = some{
        _0: value__0,
    }
    retv8 = t9
    return retv8
}

func build_point(x__1 int32, y__2 int32) point {
    var retv11 point
    var t12 point = point{
        x: x__1,
        y: y__2,
    }
    retv11 = t12
    return retv11
}

func magnitude(p__3 point) int32 {
    var retv14 int32
    var mtmp0 point = p__3
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__5 int32 = x2
    var x__4 int32 = x1
    var t15 int32 = x__4 + y__5
    retv14 = t15
    return retv14
}

func main0() int32 {
    var retv17 int32
    var mtmp3 option__int32 = make_some(5)
    var jp19 int32
    switch mtmp3.(type) {
    case some:
        var x4 int32 = mtmp3.(some)._0
        var result__6 int32 = x4
        var pt__7 point = build_point(result__6, 7)
        var t20 int32 = pt__7.x
        var mtmp5 option__int32 = some{
            _0: t20,
        }
        var jp22 int32
        switch mtmp5.(type) {
        case some:
            var x6 int32 = mtmp5.(some)._0
            var value__8 int32 = x6
            var t23 int32 = magnitude(pt__7)
            var t24 int32 = value__8 + t23
            jp22 = t24
        case none:
            jp22 = 0
        default:
            panic("non-exhaustive match")
        }
        jp19 = jp22
        retv17 = jp19
        return retv17
    case none:
        jp19 = 0
        retv17 = jp19
        return retv17
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
