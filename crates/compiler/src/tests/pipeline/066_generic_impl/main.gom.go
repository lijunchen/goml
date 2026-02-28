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
    var p1__4 Point__int32__string = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_int32__V_string(10, "hello")
    var p2__5 Point__string__string = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_string__V_string("goml", "lang")
    var p3__6 Point__string__int32 = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__swap__U_int32__V_string(p1__4)
    var x__7 int32 = p3__6.y
    var t1 string = int32_to_string(x__7)
    string_println(t1)
    var x2__8 string = _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__get_x__U_string__V_string(p2__5)
    var t2 struct{} = string_println(x2__8)
    return t2
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_int32__V_string(x__0 int32, y__1 string) Point__int32__string {
    var t3 Point__int32__string = Point__int32__string{
        x: x__0,
        y: y__1,
    }
    return t3
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__new__U_string__V_string(x__0 string, y__1 string) Point__string__string {
    var t4 Point__string__string = Point__string__string{
        x: x__0,
        y: y__1,
    }
    return t4
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__swap__U_int32__V_string(self__2 Point__int32__string) Point__string__int32 {
    var t5 string = self__2.y
    var t6 int32 = self__2.x
    var t7 Point__string__int32 = Point__string__int32{
        x: t5,
        y: t6,
    }
    return t7
}

func _goml_inherent_Point_Point_x5b_U_x2c_V_x5d__get_x__U_string__V_string(self__3 Point__string__string) string {
    var t8 string = self__3.x
    return t8
}

func main() {
    main0()
}
