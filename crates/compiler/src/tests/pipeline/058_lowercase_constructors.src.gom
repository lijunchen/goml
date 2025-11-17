package main

func missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

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
    var ret9 option__int32
    ret9 = some{
        _0: value__0,
    }
    return ret9
}

func build_point(x__1 int32, y__2 int32) point {
    var ret10 point
    ret10 = point{
        x: x__1,
        y: y__2,
    }
    return ret10
}

func magnitude(p__3 point) int32 {
    var ret11 int32
    var mtmp0 point = p__3
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__5 int32 = x2
    var x__4 int32 = x1
    ret11 = x__4 + y__5
    return ret11
}

func main0() int32 {
    var ret12 int32
    var mtmp3 option__int32 = make_some(5)
    switch mtmp3 := mtmp3.(type) {
    case some:
        var x4 int32 = mtmp3._0
        var result__6 int32 = x4
        var pt__7 point = build_point(result__6, 7)
        var t7 int32 = pt__7.x
        var mtmp5 option__int32 = some{
            _0: t7,
        }
        switch mtmp5 := mtmp5.(type) {
        case some:
            var x6 int32 = mtmp5._0
            var value__8 int32 = x6
            var t8 int32 = magnitude(pt__7)
            ret12 = value__8 + t8
        case none:
            ret12 = 0
        }
    case none:
        ret12 = missing("")
    }
    return ret12
}

func main() {
    main0()
}
