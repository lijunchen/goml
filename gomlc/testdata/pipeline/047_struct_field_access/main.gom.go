package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv69 Point
    var t70 int32 = base__0 + 1
    var t71 Point = Point{
        x: base__0,
        y: t70,
    }
    retv69 = t71
    return retv69
}

func sum_point(p__1 Point) int32 {
    var retv73 int32
    var t74 int32 = p__1.x
    var t75 int32 = p__1.y
    var t76 int32 = t74 + t75
    retv73 = t76
    return retv73
}

func main0() int32 {
    var retv78 int32
    var p__2 Point = make_point(5)
    var t79 int32 = p__2.x
    var t80 int32 = t79 + 1
    var t81 int32 = p__2.y
    var t82 int32 = t81 - 2
    var shifted__3 Point = Point{
        x: t80,
        y: t82,
    }
    var t83 int32 = shifted__3.x
    var t84 int32 = sum_point(shifted__3)
    var t85 int32 = t83 + t84
    retv78 = t85
    return retv78
}

func main() {
    main0()
}
