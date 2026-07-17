package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv62 Point
    var t63 int32 = base__0 + 1
    var t64 Point = Point{
        x: base__0,
        y: t63,
    }
    retv62 = t64
    return retv62
}

func sum_point(p__1 Point) int32 {
    var retv66 int32
    var t67 int32 = p__1.x
    var t68 int32 = p__1.y
    var t69 int32 = t67 + t68
    retv66 = t69
    return retv66
}

func main0() int32 {
    var retv71 int32
    var p__2 Point = make_point(5)
    var t72 int32 = p__2.x
    var t73 int32 = t72 + 1
    var t74 int32 = p__2.y
    var t75 int32 = t74 - 2
    var shifted__3 Point = Point{
        x: t73,
        y: t75,
    }
    var t76 int32 = shifted__3.x
    var t77 int32 = sum_point(shifted__3)
    var t78 int32 = t76 + t77
    retv71 = t78
    return retv71
}

func main() {
    main0()
}
