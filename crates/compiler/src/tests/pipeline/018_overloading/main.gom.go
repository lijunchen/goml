package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_m_trait__impl_i_Arith_i_int32_i_add(self__0 int32, other__1 int32) int32 {
    var retv27 int32
    var t28 int32 = self__0 + other__1
    retv27 = t28
    return retv27
}

func _goml_m_trait__impl_i_Arith_i_int32_i_less(self__2 int32, other__3 int32) bool {
    var retv30 bool
    var t31 bool = self__2 < other__3
    retv30 = t31
    return retv30
}

func _goml_m_trait__impl_i_Output_i_int32_i_output(self__4 int32) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__4)
    println__T_string(t33)
    return struct{}{}
}

func _goml_m_trait__impl_i_Output_i_bool_i_output(self__5 bool) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__5)
    println__T_string(t36)
    return struct{}{}
}

func main0() struct{} {
    var a__7 int32 = id__T_int32(1)
    var b__8 int32 = id__T_int32(2)
    var c__9 int32 = _goml_m_trait__impl_i_Arith_i_int32_i_add(a__7, b__8)
    _goml_m_trait__impl_i_Output_i_int32_i_output(c__9)
    var a__10 int32 = id__T_int32(3)
    var b__11 int32 = id__T_int32(4)
    var c__12 bool = _goml_m_trait__impl_i_Arith_i_int32_i_less(a__10, b__11)
    _goml_m_trait__impl_i_Output_i_bool_i_output(c__12)
    id__T_string("abc")
    id__T_bool(true)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t40 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t40)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv43 string
    var t44 string = _goml_runtime_core_int32_to_string(self__13)
    retv43 = t44
    return retv43
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv46 string
    var t47 string = _goml_runtime_core_bool_to_string(self__8)
    retv46 = t47
    return retv46
}

func id__T_int32(x__6 int32) int32 {
    var retv49 int32
    retv49 = x__6
    return retv49
}

func id__T_string(x__6 string) string {
    var retv51 string
    retv51 = x__6
    return retv51
}

func id__T_bool(x__6 bool) bool {
    var retv53 bool
    retv53 = x__6
    return retv53
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv55 string
    retv55 = self__9
    return retv55
}

func main() {
    main0()
}
