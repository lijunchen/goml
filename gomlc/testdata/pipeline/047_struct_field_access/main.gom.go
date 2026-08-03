package main

type Point struct {
    x int32
    y int32
}

func main0() int32 {
    var p__2 Point
    var inline200 int32 = 5
    var inline201 int32 = inline200 + 1
    var inline202 Point = Point{
        x: inline200,
        y: inline201,
    }
    p__2 = inline202
    var t188 int32 = p__2.x
    var t189 int32 = t188 + 1
    var t190 int32 = p__2.y
    var t191 int32 = t190 - 2
    var t193 int32
    var inline198 int32 = t189 + t191
    t193 = inline198
    var t194 int32 = t189 + t193
    return t194
}

func main() {
    main0()
}
