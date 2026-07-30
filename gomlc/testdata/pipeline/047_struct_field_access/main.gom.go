package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv109 Point
    var t110 int32 = base__0 + 1
    var t111 Point = Point{
        x: base__0,
        y: t110,
    }
    retv109 = t111
    return retv109
}

func sum_point(p__1 Point) int32 {
    var retv113 int32
    var t114 int32 = p__1.x
    var t115 int32 = p__1.y
    var t116 int32 = t114 + t115
    retv113 = t116
    return retv113
}

func main0() int32 {
    var retv118 int32
    var p__2 Point = make_point(5)
    var t119 int32 = p__2.x
    var t120 int32 = t119 + 1
    var t121 int32 = p__2.y
    var t122 int32 = t121 - 2
    var shifted__3 Point = Point{
        x: t120,
        y: t122,
    }
    var t123 int32 = shifted__3.x
    var t124 int32 = sum_point(shifted__3)
    var t125 int32 = t123 + t124
    retv118 = t125
    return retv118
}

func main() {
    main0()
}
