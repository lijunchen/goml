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
    var t7 option__int32
    t7 = some{
        _0: value__0,
    }
    return t7
}

func build_point(x__1 int32, y__2 int32) point {
    var t8 point
    t8 = point{
        x: x__1,
        y: y__2,
    }
    return t8
}

func magnitude(p__3 point) int32 {
    var mtmp0 point
    var x1 int32
    var x2 int32
    var y__5 int32
    var x__4 int32
    var t9 int32
    mtmp0 = p__3
    x1 = mtmp0.x
    x2 = mtmp0.y
    y__5 = x2
    x__4 = x1
    t9 = x__4 + y__5
    return t9
}

func main0() int32 {
    var mtmp3 option__int32
    var jp11 int32
    var x4 int32
    var result__6 int32
    var pt__7 point
    var t12 int32
    var mtmp5 option__int32
    var jp14 int32
    var x6 int32
    var value__8 int32
    var t15 int32
    var t16 int32
    mtmp3 = make_some(5)
    switch mtmp3.(type) {
    case some:
        goto b2
    case none:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp11
    b2:
    x4 = mtmp3.(some)._0
    result__6 = x4
    pt__7 = build_point(result__6, 7)
    t12 = pt__7.x
    mtmp5 = some{
        _0: t12,
    }
    switch mtmp5.(type) {
    case some:
        goto b4
    case none:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b3:
    jp11 = jp14
    goto b1
    b4:
    x6 = mtmp5.(some)._0
    value__8 = x6
    t15 = magnitude(pt__7)
    t16 = value__8 + t15
    jp14 = t16
    goto b3
    b5:
    jp14 = 0
    goto b3
    b6:
    jp11 = 0
    goto b1
}

func main() {
    main0()
}
