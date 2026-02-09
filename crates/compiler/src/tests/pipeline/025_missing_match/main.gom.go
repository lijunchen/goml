package main

func missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func missing__unit(s string) struct{} {
    missing(s)
    var ret struct{}
    return ret
}

type Point struct {
    x int32
    y int32
}

func main0() struct{} {
    var ret3 struct{}
    var p0__0 Point = Point{
        x: 0,
        y: 0,
    }
    var mtmp0 Point = p0__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    switch x2 {
    case 0:
        switch x1 {
        case 0:
            ret3 = struct{}{}
        default:
            ret3 = missing__unit("")
        }
    default:
        ret3 = missing__unit("")
    }
    return ret3
}

func main() {
    main0()
}
