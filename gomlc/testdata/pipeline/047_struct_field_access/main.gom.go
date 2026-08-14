package main

type Point struct {
    x int32
    y int32
}

func main0() int32 {
    var p__2 Point
    var inline210 int32 = 5
    var inline211 int32 = inline210 + 1
    var inline212 Point = Point{
        x: inline210,
        y: inline211,
    }
    p__2 = inline212
    var t198 int32 = p__2.x
    var t199 int32 = t198 + 1
    var t200 int32 = p__2.y
    var t201 int32 = t200 - 2
    var t203 int32
    var inline208 int32 = t199 + t201
    t203 = inline208
    var t204 int32 = t199 + t203
    return t204
}

func main() {
    main0()
}
