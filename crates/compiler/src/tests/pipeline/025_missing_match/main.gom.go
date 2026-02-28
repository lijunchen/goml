package main

type Point struct {
    x int32
    y int32
}

func main0() struct{} {
    var p0__0 Point
    var mtmp0 Point
    var x1 int32
    var x2 int32
    var y__2 int32
    var x__1 int32
    var mtmp3 int32
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            p0__0 = Point{
                x: 0,
                y: 0,
            }
            mtmp0 = p0__0
            x1 = mtmp0.x
            x2 = mtmp0.y
            y__2 = x2
            x__1 = x1
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
