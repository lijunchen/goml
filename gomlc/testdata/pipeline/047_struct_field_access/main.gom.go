package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv156 Point
    var t157 int32 = base__0 + 1
    var t158 Point = Point{
        x: base__0,
        y: t157,
    }
    retv156 = t158
    return retv156
}

func sum_point(p__1 Point) int32 {
    var retv160 int32
    var t161 int32 = p__1.x
    var t162 int32 = p__1.y
    var t163 int32 = t161 + t162
    retv160 = t163
    return retv160
}

func main0() int32 {
    var retv165 int32
    var p__2 Point = make_point(5)
    var t166 int32 = p__2.x
    var t167 int32 = t166 + 1
    var t168 int32 = p__2.y
    var t169 int32 = t168 - 2
    var shifted__3 Point = Point{
        x: t167,
        y: t169,
    }
    var t170 int32 = shifted__3.x
    var t171 int32 = sum_point(shifted__3)
    var t172 int32 = t170 + t171
    retv165 = t172
    return retv165
}

func main() {
    main0()
}
