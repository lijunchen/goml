package main

type Point struct {
    x int32
    y int32
}

func main0() int32 {
    var p__2 Point
    var inline195 int32 = 5
    var inline196 int32 = inline195 + 1
    var inline197 Point = Point{
        x: inline195,
        y: inline196,
    }
    p__2 = inline197
    var t183 int32 = p__2.x
    var t184 int32 = t183 + 1
    var t185 int32 = p__2.y
    var t186 int32 = t185 - 2
    var t188 int32
    var inline193 int32 = t184 + t186
    t188 = inline193
    var t189 int32 = t184 + t188
    return t189
}

func main() {
    main0()
}
