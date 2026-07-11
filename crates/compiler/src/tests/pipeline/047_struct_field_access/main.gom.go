package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv5 Point
    var t6 int32 = base__0 + 1
    var t7 Point = Point{
        x: base__0,
        y: t6,
    }
    retv5 = t7
    return retv5
}

func sum_point(p__1 Point) int32 {
    var retv9 int32
    var t10 int32 = p__1.x
    var t11 int32 = p__1.y
    var t12 int32 = t10 + t11
    retv9 = t12
    return retv9
}

func main0() int32 {
    var retv14 int32
    var p__2 Point = make_point(5)
    var t15 int32 = p__2.x
    var t16 int32 = t15 + 1
    var t17 int32 = p__2.y
    var t18 int32 = t17 - 2
    var shifted__3 Point = Point{
        x: t16,
        y: t18,
    }
    var t19 int32 = shifted__3.x
    var t20 int32 = sum_point(shifted__3)
    var t21 int32 = t19 + t20
    retv14 = t21
    return retv14
}

func main() {
    main0()
}
