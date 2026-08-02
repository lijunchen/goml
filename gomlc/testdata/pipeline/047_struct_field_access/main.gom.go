package main

type Point struct {
    x int32
    y int32
}

func main0() int32 {
    var p__2 Point
    var inline178 int32 = 5
    var inline179 int32 = inline178 + 1
    var inline180 Point = Point{
        x: inline178,
        y: inline179,
    }
    p__2 = inline180
    var t166 int32 = p__2.x
    var t167 int32 = t166 + 1
    var t168 int32 = p__2.y
    var t169 int32 = t168 - 2
    var t171 int32
    var inline176 int32 = t167 + t169
    t171 = inline176
    var t172 int32 = t167 + t171
    return t172
}

func main() {
    main0()
}
