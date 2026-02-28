package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var t0 int32
    var t1 Point
    t0 = base__0 + 1
    t1 = Point{
        x: base__0,
        y: t0,
    }
    return t1
}

func sum_point(p__1 Point) int32 {
    var t2 int32
    var t3 int32
    var t4 int32
    t2 = p__1.x
    t3 = p__1.y
    t4 = t2 + t3
    return t4
}

func main0() int32 {
    var p__2 Point
    var t5 int32
    var t6 int32
    var t7 int32
    var t8 int32
    var shifted__3 Point
    var t9 int32
    var t10 int32
    var t11 int32
    p__2 = make_point(5)
    t5 = p__2.x
    t6 = t5 + 1
    t7 = p__2.y
    t8 = t7 - 2
    shifted__3 = Point{
        x: t6,
        y: t8,
    }
    t9 = shifted__3.x
    t10 = sum_point(shifted__3)
    t11 = t9 + t10
    return t11
}

func main() {
    main0()
}
