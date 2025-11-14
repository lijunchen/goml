package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var ret9 Point
    var t0 int32 = base__0 + 1
    ret9 = Point{
        x: base__0,
        y: t0,
    }
    return ret9
}

func sum_point(p__1 Point) int32 {
    var ret10 int32
    var t1 int32 = p__1.x
    var t2 int32 = p__1.y
    ret10 = t1 + t2
    return ret10
}

func main0() int32 {
    var ret11 int32
    var p__2 Point = make_point(5)
    var t4 int32 = p__2.x
    var t3 int32 = t4 + 1
    var t6 int32 = p__2.y
    var t5 int32 = t6 - 2
    var shifted__3 Point = Point{
        x: t3,
        y: t5,
    }
    var t7 int32 = shifted__3.x
    var t8 int32 = sum_point(shifted__3)
    ret11 = t7 + t8
    return ret11
}

func main() {
    main0()
}
