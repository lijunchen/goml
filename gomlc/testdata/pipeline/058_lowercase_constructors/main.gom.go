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
    var retv163 Maybe__int32
    var t164 Maybe__int32 = Some{
        _0: value__0,
    }
    retv163 = t164
    return retv163
}

func build_point(x__1 int32, y__2 int32) Point {
    var retv166 Point
    var t167 Point = Point{
        x: x__1,
        y: y__2,
    }
    retv166 = t167
    return retv166
}

func magnitude(p__3 Point) int32 {
    var retv169 int32
    var mtmp155 Point = p__3
    var x156 int32 = mtmp155.x
    var x157 int32 = mtmp155.y
    var y__5 int32 = x157
    var x__4 int32 = x156
    var t170 int32 = x__4 + y__5
    retv169 = t170
    return retv169
}

func main0() int32 {
    var retv172 int32
    var mtmp158 Maybe__int32 = make_some(5)
    var jp174 int32
    switch mtmp158.(type) {
    case Some:
        var x159 int32 = mtmp158.(Some)._0
        var result__6 int32 = x159
        var pt__7 Point = build_point(result__6, 7)
        var t175 int32 = pt__7.x
        var mtmp160 Maybe__int32 = Some{
            _0: t175,
        }
        var jp177 int32
        switch mtmp160.(type) {
        case Some:
            var x161 int32 = mtmp160.(Some)._0
            var value__8 int32 = x161
            var t178 int32 = magnitude(pt__7)
            var t179 int32 = value__8 + t178
            jp177 = t179
        case None:
            jp177 = 0
        default:
            panic("non-exhaustive match")
        }
        jp174 = jp177
    case None:
        jp174 = 0
    default:
        panic("non-exhaustive match")
    }
    retv172 = jp174
    return retv172
}

func main() {
    main0()
}
