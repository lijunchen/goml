package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv65 Point
    var t66 int32 = base__0 + 1
    var t67 Point = Point{
        x: base__0,
        y: t66,
    }
    retv65 = t67
    return retv65
}

func sum_point(p__1 Point) int32 {
    var retv69 int32
    var t70 int32 = p__1.x
    var t71 int32 = p__1.y
    var t72 int32 = t70 + t71
    retv69 = t72
    return retv69
}

func main0() int32 {
    var retv74 int32
    var p__2 Point = make_point(5)
    var t75 int32 = p__2.x
    var t76 int32 = t75 + 1
    var t77 int32 = p__2.y
    var t78 int32 = t77 - 2
    var shifted__3 Point = Point{
        x: t76,
        y: t78,
    }
    var t79 int32 = shifted__3.x
    var t80 int32 = sum_point(shifted__3)
    var t81 int32 = t79 + t80
    retv74 = t81
    return retv74
}

func main() {
    main0()
}
