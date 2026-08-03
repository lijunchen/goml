package main

type Point struct {
    x int32
    y int32
}

func main0() int32 {
    var p__2 Point
    var inline159 int32 = 5
    var inline160 int32 = inline159 + 1
    var inline161 Point = Point{
        x: inline159,
        y: inline160,
    }
    p__2 = inline161
    var t147 int32 = p__2.x
    var t148 int32 = t147 + 1
    var t149 int32 = p__2.y
    var t150 int32 = t149 - 2
    var t152 int32
    var inline157 int32 = t148 + t150
    t152 = inline157
    var t153 int32 = t148 + t152
    return t153
}

func main() {
    main0()
}
