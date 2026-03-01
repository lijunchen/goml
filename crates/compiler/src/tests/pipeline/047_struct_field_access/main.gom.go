package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv1 Point
    var t2 int32 = base__0 + 1
    var t3 Point = Point{
        x: base__0,
        y: t2,
    }
    retv1 = t3
    return retv1
}

func sum_point(p__1 Point) int32 {
    var retv5 int32
    var t6 int32 = p__1.x
    var t7 int32 = p__1.y
    var t8 int32 = t6 + t7
    retv5 = t8
    return retv5
}

func main0() int32 {
    var retv10 int32
    var p__2 Point = make_point(5)
    var t11 int32 = p__2.x
    var t12 int32 = t11 + 1
    var t13 int32 = p__2.y
    var t14 int32 = t13 - 2
    var shifted__3 Point = Point{
        x: t12,
        y: t14,
    }
    var t15 int32 = shifted__3.x
    var t16 int32 = sum_point(shifted__3)
    var t17 int32 = t15 + t16
    retv10 = t17
    return retv10
}

func main() {
    main0()
}
