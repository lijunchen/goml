package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv8 Point
    var t9 int32 = base__0 + 1
    var t10 Point = Point{
        x: base__0,
        y: t9,
    }
    retv8 = t10
    return retv8
}

func sum_point(p__1 Point) int32 {
    var retv12 int32
    var t13 int32 = p__1.x
    var t14 int32 = p__1.y
    var t15 int32 = t13 + t14
    retv12 = t15
    return retv12
}

func main0() int32 {
    var retv17 int32
    var p__2 Point = make_point(5)
    var t18 int32 = p__2.x
    var t19 int32 = t18 + 1
    var t20 int32 = p__2.y
    var t21 int32 = t20 - 2
    var shifted__3 Point = Point{
        x: t19,
        y: t21,
    }
    var t22 int32 = shifted__3.x
    var t23 int32 = sum_point(shifted__3)
    var t24 int32 = t22 + t23
    retv17 = t24
    return retv17
}

func main() {
    main0()
}
