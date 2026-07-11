package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv23 Point
    var t24 int32 = base__0 + 1
    var t25 Point = Point{
        x: base__0,
        y: t24,
    }
    retv23 = t25
    return retv23
}

func sum_point(p__1 Point) int32 {
    var retv27 int32
    var t28 int32 = p__1.x
    var t29 int32 = p__1.y
    var t30 int32 = t28 + t29
    retv27 = t30
    return retv27
}

func main0() int32 {
    var retv32 int32
    var p__2 Point = make_point(5)
    var t33 int32 = p__2.x
    var t34 int32 = t33 + 1
    var t35 int32 = p__2.y
    var t36 int32 = t35 - 2
    var shifted__3 Point = Point{
        x: t34,
        y: t36,
    }
    var t37 int32 = shifted__3.x
    var t38 int32 = sum_point(shifted__3)
    var t39 int32 = t37 + t38
    retv32 = t39
    return retv32
}

func main() {
    main0()
}
