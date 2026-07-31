package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv153 Point
    var t154 int32 = base__0 + 1
    var t155 Point = Point{
        x: base__0,
        y: t154,
    }
    retv153 = t155
    return retv153
}

func sum_point(p__1 Point) int32 {
    var retv157 int32
    var t158 int32 = p__1.x
    var t159 int32 = p__1.y
    var t160 int32 = t158 + t159
    retv157 = t160
    return retv157
}

func main0() int32 {
    var retv162 int32
    var p__2 Point = make_point(5)
    var t163 int32 = p__2.x
    var t164 int32 = t163 + 1
    var t165 int32 = p__2.y
    var t166 int32 = t165 - 2
    var shifted__3 Point = Point{
        x: t164,
        y: t166,
    }
    var t167 int32 = shifted__3.x
    var t168 int32 = sum_point(shifted__3)
    var t169 int32 = t167 + t168
    retv162 = t169
    return retv162
}

func main() {
    main0()
}
