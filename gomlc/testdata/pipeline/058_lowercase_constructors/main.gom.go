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
    var retv116 Maybe__int32
    var t117 Maybe__int32 = Some{
        _0: value__0,
    }
    retv116 = t117
    return retv116
}

func build_point(x__1 int32, y__2 int32) Point {
    var retv119 Point
    var t120 Point = Point{
        x: x__1,
        y: y__2,
    }
    retv119 = t120
    return retv119
}

func magnitude(p__3 Point) int32 {
    var retv122 int32
    var mtmp108 Point = p__3
    var x109 int32 = mtmp108.x
    var x110 int32 = mtmp108.y
    var y__5 int32 = x110
    var x__4 int32 = x109
    var t123 int32 = x__4 + y__5
    retv122 = t123
    return retv122
}

func main0() int32 {
    var retv125 int32
    var mtmp111 Maybe__int32 = make_some(5)
    var jp127 int32
    switch mtmp111.(type) {
    case Some:
        var x112 int32 = mtmp111.(Some)._0
        var result__6 int32 = x112
        var pt__7 Point = build_point(result__6, 7)
        var t128 int32 = pt__7.x
        var mtmp113 Maybe__int32 = Some{
            _0: t128,
        }
        var jp130 int32
        switch mtmp113.(type) {
        case Some:
            var x114 int32 = mtmp113.(Some)._0
            var value__8 int32 = x114
            var t131 int32 = magnitude(pt__7)
            var t132 int32 = value__8 + t131
            jp130 = t132
        case None:
            jp130 = 0
        default:
            panic("non-exhaustive match")
        }
        jp127 = jp130
    case None:
        jp127 = 0
    default:
        panic("non-exhaustive match")
    }
    retv125 = jp127
    return retv125
}

func main() {
    main0()
}
