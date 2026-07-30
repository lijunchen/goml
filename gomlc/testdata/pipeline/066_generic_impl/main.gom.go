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
    var t110 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__7)
    println__T_string(t110)
    var x2__8 string = _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_get__x____U__string____V__string(p2__5)
    println__T_string(x2__8)
    return struct{}{}
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__int32____V__string(x__0 int32, y__1 string) Point__int32__string {
    var retv113 Point__int32__string
    var t114 Point__int32__string = Point__int32__string{
        x: x__0,
        y: y__1,
    }
    retv113 = t114
    return retv113
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_new____U__string____V__string(x__0 string, y__1 string) Point__string__string {
    var retv116 Point__string__string
    var t117 Point__string__string = Point__string__string{
        x: x__0,
        y: y__1,
    }
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_swap____U__int32____V__string(self__2 Point__int32__string) Point__string__int32 {
    var retv119 Point__string__int32
    var t120 string = self__2.y
    var t121 int32 = self__2.x
    var t122 Point__string__int32 = Point__string__int32{
        x: t120,
        y: t121,
    }
    retv119 = t122
    return retv119
}

func println__T_string(value__1 string) struct{} {
    var t124 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t124)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv127 string
    var t128 string = _goml_runtime_core_int32_to_string(self__6)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_Point_i_Point_l_U_c_V_r__i_get__x____U__string____V__string(self__3 Point__string__string) string {
    var retv130 string
    var t131 string = self__3.x
    retv130 = t131
    return retv130
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv133 string
    retv133 = self__38
    return retv133
}

func main() {
    main0()
}
