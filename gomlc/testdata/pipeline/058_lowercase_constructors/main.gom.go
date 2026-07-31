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
    var retv160 Maybe__int32
    var t161 Maybe__int32 = Some{
        _0: value__0,
    }
    retv160 = t161
    return retv160
}

func build_point(x__1 int32, y__2 int32) Point {
    var retv163 Point
    var t164 Point = Point{
        x: x__1,
        y: y__2,
    }
    retv163 = t164
    return retv163
}

func magnitude(p__3 Point) int32 {
    var retv166 int32
    var mtmp152 Point = p__3
    var x153 int32 = mtmp152.x
    var x154 int32 = mtmp152.y
    var y__5 int32 = x154
    var x__4 int32 = x153
    var t167 int32 = x__4 + y__5
    retv166 = t167
    return retv166
}

func main0() int32 {
    var retv169 int32
    var mtmp155 Maybe__int32 = make_some(5)
    var jp171 int32
    switch mtmp155.(type) {
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        var result__6 int32 = x156
        var pt__7 Point = build_point(result__6, 7)
        var t172 int32 = pt__7.x
        var mtmp157 Maybe__int32 = Some{
            _0: t172,
        }
        var jp174 int32
        switch mtmp157.(type) {
        case Some:
            var x158 int32 = mtmp157.(Some)._0
            var value__8 int32 = x158
            var t175 int32 = magnitude(pt__7)
            var t176 int32 = value__8 + t175
            jp174 = t176
        case None:
            jp174 = 0
        default:
            panic("non-exhaustive match")
        }
        jp171 = jp174
    case None:
        jp171 = 0
    default:
        panic("non-exhaustive match")
    }
    retv169 = jp171
    return retv169
}

func main() {
    main0()
}
