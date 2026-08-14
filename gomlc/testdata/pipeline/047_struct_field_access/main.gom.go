package main

type Point struct {
    x int32
    y int32
}

func main0() int32 {
    var p__2 Point
    var inline205 int32 = 5
    var inline206 int32 = inline205 + 1
    var inline207 Point = Point{
        x: inline205,
        y: inline206,
    }
    p__2 = inline207
    var t193 int32 = p__2.x
    var t194 int32 = t193 + 1
    var t195 int32 = p__2.y
    var t196 int32 = t195 - 2
    var t198 int32
    var inline203 int32 = t194 + t196
    t198 = inline203
    var t199 int32 = t194 + t198
    return t199
}

func main() {
    main0()
}
