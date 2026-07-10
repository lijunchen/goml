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
    var p1__4 Point__int32__string = _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__int32____V__string(10, "hello")
    var p2__5 Point__string__string = _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__string____V__string("goml", "lang")
    var p3__6 Point__string__int32 = _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_swap____U__int32____V__string(p1__4)
    var x__7 int32 = p3__6.y
    var t2 string = int32_to_string(x__7)
    println__T_string(t2)
    var x2__8 string = _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_get__x____U__string____V__string(p2__5)
    println__T_string(x2__8)
    return struct{}{}
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__int32____V__string(x__0 int32, y__1 string) Point__int32__string {
    var retv5 Point__int32__string
    var t6 Point__int32__string = Point__int32__string{
        x: x__0,
        y: y__1,
    }
    retv5 = t6
    return retv5
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__string____V__string(x__0 string, y__1 string) Point__string__string {
    var retv8 Point__string__string
    var t9 Point__string__string = Point__string__string{
        x: x__0,
        y: y__1,
    }
    retv8 = t9
    return retv8
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_swap____U__int32____V__string(self__2 Point__int32__string) Point__string__int32 {
    var retv11 Point__string__int32
    var t12 string = self__2.y
    var t13 int32 = self__2.x
    var t14 Point__string__int32 = Point__string__int32{
        x: t12,
        y: t13,
    }
    retv11 = t14
    return retv11
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_get__x____U__string____V__string(self__3 Point__string__string) string {
    var retv18 string
    var t19 string = self__3.x
    retv18 = t19
    return retv18
}

func main() {
    main0()
}
