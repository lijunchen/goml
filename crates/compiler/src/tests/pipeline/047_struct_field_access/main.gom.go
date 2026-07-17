package main

type Point struct {
    x int32
    y int32
}

func make_point(base__0 int32) Point {
    var retv59 Point
    var t60 int32 = base__0 + 1
    var t61 Point = Point{
        x: base__0,
        y: t60,
    }
    retv59 = t61
    return retv59
}

func sum_point(p__1 Point) int32 {
    var retv63 int32
    var t64 int32 = p__1.x
    var t65 int32 = p__1.y
    var t66 int32 = t64 + t65
    retv63 = t66
    return retv63
}

func main0() int32 {
    var retv68 int32
    var p__2 Point = make_point(5)
    var t69 int32 = p__2.x
    var t70 int32 = t69 + 1
    var t71 int32 = p__2.y
    var t72 int32 = t71 - 2
    var shifted__3 Point = Point{
        x: t70,
        y: t72,
    }
    var t73 int32 = shifted__3.x
    var t74 int32 = sum_point(shifted__3)
    var t75 int32 = t73 + t74
    retv68 = t75
    return retv68
}

func main() {
    main0()
}
