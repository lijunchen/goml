package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Point__int32__string struct {
    x int32
    y string
}

type Point__string__string struct {
    x string
    y string
}

type Point__string__int32 struct {
    x string
    y int32
}

func main0() struct{} {
    var p1__4 Point__int32__string
    var p2__5 Point__string__string
    var p3__6 Point__string__int32
    var x__7 int32
    var t1 string
    var _wild0 struct{}
    var x2__8 string
    var t2 struct{}
    _ = _wild0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            p1__4 = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_int32__V_string(10, "hello")
            p2__5 = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_string__V_string("goml", "lang")
            p3__6 = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__swap__U_int32__V_string(p1__4)
            x__7 = p3__6.y
            t1 = int32_to_string(x__7)
            string_println(t1)
            x2__8 = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__get_x__U_string__V_string(p2__5)
            t2 = string_println(x2__8)
            return t2
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_int32__V_string(x__0 int32, y__1 string) Point__int32__string {
    var t3 Point__int32__string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t3 = Point__int32__string{
                x: x__0,
                y: y__1,
            }
            return t3
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_string__V_string(x__0 string, y__1 string) Point__string__string {
    var t4 Point__string__string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = Point__string__string{
                x: x__0,
                y: y__1,
            }
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__swap__U_int32__V_string(self__2 Point__int32__string) Point__string__int32 {
    var t5 string
    var t6 int32
    var t7 Point__string__int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t5 = self__2.y
            t6 = self__2.x
            t7 = Point__string__int32{
                x: t5,
                y: t6,
            }
            return t7
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__get_x__U_string__V_string(self__3 Point__string__string) string {
    var t8 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t8 = self__3.x
            return t8
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
