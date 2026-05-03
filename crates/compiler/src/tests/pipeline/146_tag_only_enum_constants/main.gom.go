package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Light int32

const (
    Light_Red Light = 0
    Yellow Light = 1
    Green Light = 2
)

type Paint int32

const (
    Paint_Red Paint = 0
    Blue Paint = 1
)

func light_code(light__0 Light) int32 {
    var retv4 int32
    var jp6 int32
    switch light__0 {
    case Light_Red:
        jp6 = 10
    case Yellow:
        jp6 = 20
    case Green:
        jp6 = 30
    default:
        panic("non-exhaustive match")
    }
    retv4 = jp6
    return retv4
}

func paint_code(paint__1 Paint) int32 {
    var retv8 int32
    var jp10 int32
    switch paint__1 {
    case Paint_Red:
        jp10 = 1
    case Blue:
        jp10 = 2
    default:
        panic("non-exhaustive match")
    }
    retv8 = jp10
    return retv8
}

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t12 int32 = light_code(light__2)
    var t13 string = int32_to_string(t12)
    println__T_string(t13)
    var t14 int32 = paint_code(paint__3)
    var t15 string = int32_to_string(t14)
    println__T_string(t15)
    var t16 int32 = light_code(Green)
    var t17 string = int32_to_string(t16)
    println__T_string(t17)
    var t18 int32 = paint_code(Blue)
    var t19 string = int32_to_string(t18)
    println__T_string(t19)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
