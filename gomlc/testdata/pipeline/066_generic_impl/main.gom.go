package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
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
    var t157 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__7)
    println__T_string(t157)
    var x2__8 string = _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_get__x____U__string____V__string(p2__5)
    println__T_string(x2__8)
    return struct{}{}
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__int32____V__string(x__0 int32, y__1 string) Point__int32__string {
    var t161 Point__int32__string = Point__int32__string{
        x: x__0,
        y: y__1,
    }
    return t161
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__string____V__string(x__0 string, y__1 string) Point__string__string {
    var t164 Point__string__string = Point__string__string{
        x: x__0,
        y: y__1,
    }
    return t164
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_swap____U__int32____V__string(self__2 Point__int32__string) Point__string__int32 {
    var t167 string = self__2.y
    var t168 int32 = self__2.x
    var t169 Point__string__int32 = Point__string__int32{
        x: t167,
        y: t168,
    }
    return t169
}

func println__T_string(value__1 string) struct{} {
    var t171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t171)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t175 string = _goml_runtime_core_int32_to_string(self__6)
    return t175
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_get__x____U__string____V__string(self__3 Point__string__string) string {
    var t178 string = self__3.x
    return t178
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
