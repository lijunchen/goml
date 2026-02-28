package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var t0 int32 = base__0 + 1
    var t1 Point = Point{
        x: base__0,
        y: t0,
    }
    return t1
}

func sum_point(p__1 Point) int32 {
    var t2 int32 = p__1.x
    var t3 int32 = p__1.y
    var t4 int32 = t2 + t3
    return t4
}

func main0() int32 {
    var p__2 Point = make_point(5)
    var t5 int32 = p__2.x
    var t6 int32 = t5 + 1
    var t7 int32 = p__2.y
    var t8 int32 = t7 - 2
    var shifted__3 Point = Point{
        x: t6,
        y: t8,
    }
    var t9 int32 = shifted__3.x
    var t10 int32 = sum_point(shifted__3)
    var t11 int32 = t9 + t10
    return t11
}

func main() {
    main0()
}
